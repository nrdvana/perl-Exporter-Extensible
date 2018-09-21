package Exporter::Extensible;

use strict;
use warnings;

our %EXPORT_PKG_CACHE;
our %EXPORT_TAGS_PKG_CACHE;

our %EXPORT= (
	-exporter_setup => [ 'exporter_setup', 1 ],
);

our %sigil_to_type= (
	'$' => 'SCALAR',
	'@' => 'ARRAY',
	'%' => 'HASH',
	'*' => 'GLOB',
	'&' => 'CODE',
	''  => 'CODE',
	'-' => 'CODE',
);
our %sigil_to_generator_prefix= (
	'$' => '_generateSCALAR_',
	'@' => '_generateARRAY_',
	'%' => '_generateHASH_',
	'*' => '_generateGLOB_',
	'&' => '_generate_',
	''  => '_generate_',
);

sub _croak { require Carp; goto &Carp::croak; }

sub import {
	# Can be called as class method or instance method.
	my $self= shift;
	$self= bless {}, $self unless ref $self;
	
	# If first argument is a hashref, use it as global configuration for this operation
	%$self= (%$self, %{+shift}) if ref $_[0] eq 'HASH';
	
	# Capture caller() unless 'into' was already indicated.
	my $into= $self->{into} ||= caller;
	# If caller wants scope option, make sure it has been blessed
	bless $self->{scope}, 'Exporter::Extensible::UnimportScopeGuard'
		if ref $self->{scope} eq 'SCALAR';
	
	# Cache some lookups
	my $export_sym= ($EXPORT_PKG_CACHE{ref $self} ||= {});
	
	for (my $i= 0; $i < @_;) {
		my $symbol= $_[$i++];
		my ($sigil, $name)= ($symbol =~ /^([-:\$\@\%\*]?)(.*)/); # should always match
		# If followed by a hashref, add those options to the current ones.
		# But, not if it is an -option, because -option might use hashrefs for other purposes.
		local %$self= ( %$self, %{$_[$i++]} )
			if ref $_[$i] eq 'HASH' and $sigil ne '-';
		
		# If it is a tag, then recursively call import on that list
		if ($sigil eq ':') {
			my @tag_list= $self->exporter_get_tag_members($name);
			$self->import(@tag_list) if @tag_list; # only if the tags weren't all excluded
			next;
		}
		# Else, it is an option or plain symbol to be exported
		# Check current package cache first, else do the full lookup.
		my $ref= (exists $export_sym->{$symbol}? $export_sym->{$symbol} : $self->exporter_get_inherited($symbol))
			or _croak("'$symbol' is not exported by ".ref($self));
		# Generators are a ref ref to a method name or coderef.
		if (ref $ref eq 'REF') {
			$ref= $$ref;
			$ref= $$ref unless ref $ref eq 'CODE';
			$ref= $self->$ref($symbol);
		}
		
		# If it starts with '-', it is an option, and might consume additional args
		if ($sigil eq '-') {
			my ($method, $count)= @$ref;
			if ($count eq '*') {
				my $consumed= $self->$method(@_[$i..$#_]);
				$consumed =~ /^[0-9]+/ or _croak("Method $method in ".ref($self)." must return a number of arguments consumed");
				$i += $consumed;
			} else {
				$self->$method(@_[$i..($i+$count-1)]);
				$i += $count;
			}
			next;
		}
		
		no strict 'refs';
		no warnings 'uninitialized','redefine';
		my $dest= $self->{prefix}.(defined $self->{-as}? $self->{-as} : $name).$self->{suffix};
		if (ref $into eq 'HASH') {
			$into->{$dest}= $ref;
			next;
		}
		$dest= $into.'::'.$dest;
		if (exists &$dest and ref $symbol eq 'CODE' and \&$dest != $symbol) {
			my $r= $self->{replace} || $self->{-replace};
			if (!$r || $r eq 'carp' || $r eq 'warn') {
				_carp("Overwriting existing sub '$dest' with sub '$name' exported by ".ref($self));
			}
			elsif ($r eq 'croak' || $r eq 'fatal' || $r eq 'die') {
				_croak("Refusing to overwrite existing sub '$dest' with sub '$name' exported by ".ref($self));
			}
		}
		$self->{un}? $self->exporter_unimport_ref($dest, $ref) : (*$dest= $ref);
		#print "dest= ".*$dest." ref=$ref\n";
		push @{${$self->{scope}}}, $dest, $ref if $self->{scope};
	}
}

sub unimport {
	# If first option is a hashref (global options), merge that with { un => 1 }
	my %opts= ( (ref $_[1] eq 'HASH'? %{splice(@_,1,1)} : () ), un => 1 );
	# Use this as the global options
	splice @_, 1, 0, \%opts;
	goto $_[0]->can('import'); # to preserve caller
}

sub import_into {
	shift->import({ into => shift, (ref $_[0] eq 'HASH'? %{+shift} : ()) }, @_);
}

sub exporter_unimport_ref {
	my ($self, $full_name, $ref)= @_;
	no strict 'refs';
	my ($stashname, $name)= ($full_name =~ /(.*:)([^:]+)$/);
	my $stash= \%$stashname;
	if (ref $ref eq 'GLOB') {
		# If the value we installed is no longer there, do nothing
		return if *$ref ne ($stash->{$name}||'');
		delete $stash->{$name};
	}
	else {
		# If the value we installed is no longer there, do nothing
		return if $ref != (*{$full_name}{ref $ref}||0);
		# Remove old typeglob, then copy all slots except reftype back to that typeglob name
		my $old= delete $stash->{$name};
		($_ ne ref $ref) && *{$old}{$_} && (*$full_name= *{$old}{$_})
			for qw( SCALAR HASH ARRAY CODE IO );
	}
}

sub exporter_register_symbol {
	my ($class, $export_name, $ref)= @_;
	$class= ref($class)||$class;
	$ref ||= $class->_exporter_get_ref_to_package_var($export_name)
		or _croak("Symbol $export_name not found in package $class");
	no strict 'refs';
	${$class.'::EXPORT'}{$export_name}= $ref;
}

sub exporter_get_inherited {
	my ($self, $sym)= @_;
	my $class= ref($self)||$self;
	# Make the common case fast.
	return $EXPORT_PKG_CACHE{$class}{$sym}
		if exists $EXPORT_PKG_CACHE{$class}{$sym};
	# search package hierarchy
	no strict 'refs';
	for (@{ mro::get_linear_isa($class) }) {
		return $EXPORT_PKG_CACHE{$class}{$sym}= ${$_.'::EXPORT'}{$sym}
			if exists ${$_.'::EXPORT'}{$sym};
	}
	# Isn't exported.
	return undef;
}

sub exporter_register_option {
	my ($class, $option_name, $method_name, $arg_count)= @_;
	$class= ref($class)||$class;
	no strict 'refs';
	${$class.'::EXPORT'}{'-'.$option_name}= [ $method_name, $arg_count||0 ];
}

sub exporter_register_generator {
	my ($class, $export_name, $method_name)= @_;
	$class= ref($class)||$class;
	no strict 'refs';
	if ($export_name =~ /^:/) {
		${$class.'::EXPORT_TAGS'}= $method_name;
	} else {
		${$class.'::EXPORT'}{$export_name}= \\$method_name;
	}
}

sub exporter_register_tag_members {
	my ($class, $tag_name)= (shift, shift);
	$class= ref($class)||$class;
	no strict 'refs';
	push @{ ${$class.'::EXPORT_TAGS'}{$tag_name} }, @_;
}

sub exporter_get_tag_members {
	my ($self, $tagname)= @_;
	my $class= ref($self)||$self;
	# Make the common case fast
	my $list= $EXPORT_TAGS_PKG_CACHE{$class}{$tagname};
	if (!$list && !exists $EXPORT_TAGS_PKG_CACHE{$class}{$tagname}) {
		# Collect all members of this tag from any parent class, but stop at the first coderef
		my $dynamic;
		no strict 'refs';
		$list= [];
		for (@{ mro::get_linear_isa($class) }) {
			my $tag= ${$_.'::EXPORT_TAGS'}{$tagname} or next;
			if (ref $tag ne 'ARRAY') {
				push @$list, @{ $self->$tag };
				++$dynamic;
				last;
			}
			push @$list, @$tag;
		}
		$EXPORT_TAGS_PKG_CACHE{$class}{$tagname}= $list unless $dynamic;
	}
	# Apply "not" exclusions
	if (ref $self && (my $not= $self->{not} || $self->{-not})) {
		$list= [ @$list ]; # clone before removing exclusions
		# N^2 exclusion iteration isn't cool, but doing something smarter requires a
		# lot more setup that probably won't pay off for the usual tiny lists of 'not'.
		for my $filter (ref $not eq 'ARRAY'? @$not : ($not)) {
			if (!ref $filter) {
				@$list= grep $_ ne $filter, @$list;
			}
			elsif (ref $filter eq 'RegExp') {
				@$list= grep $_ !~ $filter, @$list;
			}
			elsif (ref $filter eq 'CODE') {
				@$list= grep !&$filter, @$list;
			}
			else { _croak("Unhandled 'not' filter: $filter") }
		}
	}
	@$list;
}

my %method_attrs;
sub FETCH_CODE_ATTRIBUTES {
	my ($class, $coderef)= (shift, shift);
	my $super= $class->next::can;
	return @{$method_attrs{$class}{$coderef} || []},
		($super? $super->($class, $coderef, @_) : ());
}
sub MODIFY_CODE_ATTRIBUTES {
	my ($class, $coderef)= (shift, shift);
	my @unknown= grep !$class->_exporter_process_attribute($coderef, $_), @_;
	my $super= $class->next::can;
	return $super? $super->($class, $coderef, @unknown) : @unknown;
}

sub _exporter_get_coderef_name {
	my $coderef= shift;
	# This code is borrowed from Sub::Identify
	require B;
	my $cv= B::svref_2object($coderef);
	$cv->isa('B::CV') && !$cv->GV->isa('B::SPECIAL')
		or _croak("Can't determine export name of $coderef");
	return $cv->GV->NAME;
}

sub _exporter_get_ref_to_package_var {
	my ($class, $symbol)= @_;
	my ($sigil, $name)= ($symbol =~ /^([\$\@\%\*]?)(\w+)$/)
		or _croak("'$symbol' is not an allowed variable name");
	my $reftype= $sigil_to_type{$sigil};
	no strict 'refs';
	return $reftype eq 'GLOB'? *{$class.'::'.$name} : *{$class.'::'.$name}{$reftype};
}

sub _exporter_process_attribute {
	my ($class, $coderef, $attr)= @_;
	if ($attr =~ /^Export(\(.*?\))?$/) {
		my (%tags, $subname, $export_name);
		# If given a list in parenthesees, split on space and proces each.  Else use the name of the sub itself.
		for my $token ($1? split(/\s+/, substr($1, 1, -1)) : ()) {
			if ($token =~ /^:(.*)$/) {
				$tags{$1}++; # save tags until we have the export_name
			}
			elsif ($token =~ /^\w+$/) {
				$export_name ||= $token;
				no strict 'refs';
				${$class.'::EXPORT'}{$token}= $coderef;
			}
			elsif ($token =~ /^-(\w*)(?:\(([0-9]+|\*)\))?$/) {
				$subname ||= _exporter_get_coderef_name($coderef);
				$export_name ||= length $1? $token : "-$subname";
				$class->exporter_register_option(substr($export_name,1), $subname, $2);
			}
			elsif ($token =~ /^=([\$\@\%\*]?(\w*))$/) {
				$subname ||= _exporter_get_coderef_name($coderef);
				$export_name ||= length $2? $1 : "$1$subname";
				$class->exporter_register_generator($export_name, $subname);
			}
			else {
				_croak("Invalid export notation '$token'");
			}
		}
		if (!defined $export_name) { # if list was empty or only tags...
			$export_name= _exporter_get_coderef_name($coderef);
			no strict 'refs';
			${$class.'::EXPORT'}{$export_name}= $coderef;
		}
		$class->exporter_register_tag_members($_, $export_name) for keys %tags;
		return 1;
	}
	return;
}

sub exporter_setup {
	my ($self, $version)= @_;
	no strict 'refs';
	push @{$self->{into}.'::ISA'}, ref($self);
	strict->import;
	warnings->import;
	if ($version == 1) {
		*{$self->{into}.'::export'}= \&_exporter_export_from_caller;
	}
	elsif ($version) {
		_croak("Unknown export API version $version");
	}
}

sub _exporter_export_from_caller {
	unshift @_, scalar caller;
	goto $_[0]->can('exporter_export');
}
sub exporter_export {
	my $class= shift;
	for (my $i= 0; $i < @_;) {
		my $export= $_[$i++];
		ref $export and _croak("Expected non-ref export name at argument $i");
		# If they provided the ref, capture it from arg list.
		my $ref= $_[$i++] if ref $_[$i];
		my ($is_gen, $sigil, $name, $args);
		# Common case first - ordinary functions
		if ($export =~ /^\w+$/) {
			$ref ||= $class->can($export) or _croak("Export '$export' not found in $class");
			no strict 'refs';
			${$class.'::EXPORT'}{$export}= $ref;
		}
		# Next, check for generators or variables with sigils
		elsif (($is_gen, $sigil, $name)= ($export =~ /^(=?)([\$\@\%\*]?)(\w+)$/)) {
			$ref ||= $class->_exporter_get_ref_to_package_var($export)
				unless $is_gen;
			if (!$ref) {
				my $gen= $sigil_to_generator_prefix{$sigil}.$name;
				$class->can($gen)
					or _croak("Export '$export' not found in package $class, nor a generator $gen");
				$ref= \\$gen;  # REF REF to method name
			}
			elsif ($is_gen) {
				ref $ref eq 'CODE' or _croak("Export '$export' should be followed by a generator coderef");
				my $coderef= $ref;
				$ref= \$coderef; # REF to coderef
			}
			else {
				ref $ref eq $sigil_to_type{$sigil}
					or _croak("'$export' should be $sigil_to_type{$sigil} but you supplied ".ref($ref));
			}
			no strict 'refs';
			${$class.'::EXPORT'}{$sigil.$name}= $ref;
		}
		# Tags ":foo"
		elsif (($is_gen, $name)= ($export =~ /^(=?):(\w+)$/)) {
			defined $is_gen and _croak("Tags can't be generators (yet) for $export");
			ref $ref eq 'ARRAY' or _croak("Tag name '$export' must be followed by an arrayref");
			$class->exporter_register_tag_members($name, @$ref);
		}
		# Options "-foo" or "-foo(3)"
		elsif (($name, $args)= ($export =~ /^-(\w+)(?:\(([0-9]+|\*)\))?$/)) {
			if ($ref) {
				ref $ref eq 'CODE' or (ref $ref eq 'SCALAR' and $class->can($ref= $$ref))
					or _croak("Option '$export' must be followed by coderef or method name as scalar ref");
			} else {
				$class->can($name)
					or _croak("Option '$export' defaults to a method '$name' which does not exist on $class");
				$ref= $name;
			}
			$class->exporter_register_option($name, $ref, $args);
		}
		else {
			_croak("'$export' is not a valid export syntax");
		}
	}
}

package Exporter::Extensible::UnimportScopeGuard;

sub DESTROY {
	my $self= shift;
	Exporter::Extensible->exporter_unimport_ref(splice @$self, -2)
		while @$self;
}

1;

=head1 SYNOPSIS

Define a module with exports

  package My::Utils;
  use Exporter::Extensible -exporter_setup => 1;
  
  sub util_function : Export {
    ...
  }
  sub util_fn2 : Export( foo :bar :baz :default ) { # exports as "foo"
    ...
  }
  
  our ($x, $y, $z);
  export(qw( $x $y $z ));
  
  our @GLOBAL_LIST_OF_STUFF;
  export('@STUFF' => \@GLOBAL_LIST_OF_STUFF);
  
  sub strict_and_warnings : Export(-) {
    strict->import;
    warnings->import;
  }

Derive a new module with exports from the previous one

  package My::MoreUtils;
  use My::Utils -exporter_setup => 1;
  sub util_fn3 : Export(:baz) { ... }

Use the module

  use My::MoreUtils qw( -strict_and_warnings :baz @STUFF );
  push @STUFF, foo(), util_fn3();

=head1 DESCRIPTION

As a module author, you have dozens of exporters to choose from, so I'll try to get straight to
the pros/cons of this module:

=head2 Pros

=over

=item Extend Your Module

This exporter focuses on the ability and ease of letting you "subclass" a module-with-exports to
create a derived module-with-exports.

=item Extend Behavior of C<import>

This exporter supports lots of ways to add custom processing during 'import' without needing to
dig into the implementation too much.

=item Be Lazy

This exporter supports on-demand generators for symbols, as well as tags!  So if you have a
complicated or expensive list of exports you can wait until the first time each is requested
before finding out whether it is available or loading the dependent module.

=item Advanced Features

This exporter attempts to copy useful features from other Exporters, like renaming imports
from the C<use> line, prefixes, suffixes, excluding symbols, importing to things other than
C<caller>, etc.

=item More-Than-One-Way-To-Declare-Exports

Pick your favorite.  You can use the L<export> do-what-I-mean function, method attributes, the
C<< __PACKAGE__->exporter_ ... >> API, or declare package variables similar to L<Exporter>.

=item No Non-core Dependencies

Because nobody likes extra deps forced into them.

=item Speed

(It should be fast... I haven't benchmarked yet, but it was designed with low overhead in mind)

=back

=head2 Cons

=over

=item Imposes meaning on hashrefs

Hashref arguments following a symbol name provide options for how to import that symbol.
If the first argument is a hashref it provides options to C<import> itself.

=item Imposes meaning for notation C<-NAME>

This module follows the L<Exporter> convention for symbol names but with the additional
convention that names beginning with dash C<-> are treated as requests for runtime behavior.
Additionally, it may consume the arguments that follow it, at the discresion of the module
author.  This feature is designed to feel like command-line option processing.

=item Different package variables than Exporter

I like standards where possible, but Exporter's C<@EXPORT_OK> stank.  It should be a hash, not
a list, for efficient lookups.

=item (Small) Namespace and inheritance pollution

This module defines a few API methods used for the declaration and definition of the export
process.  Exporting modules must inherit from this module, thus inheriting that API as well.
The API I<doesn't> get exported to consumers even if they request ':all', so it's only a small
worry for name collisions within the packages defining the exports.

I could have kept these meta-methods in a separate namespace, but that would defeat the goal of
"easy to extend".

If you want a pure class hierarchy for OO purposes but also export a few symbols, consider
something like this:

    package My::Class;
    package My::Class::Exports {
      use Exporter::Extensible -exporter_setup => 1;
      ...
    }
    sub import { My::Class::Exports->import_into(scalar caller, @_) }

=back

=head1 CONSUMER API

=head1 AUTHOR API

The only thing you need to do to build on this module are to inherit from it,
and to declare your exports in the variables C<%EXPORT> and C<%EXPORT_TAGS>.
The quickest way to do that is:

  package My::Module;
  use Exporter::Extensible -exporter_setup => 1;
  export(...);

Those lines are shorthand for:

  package My::Module;
  use strict;
  use warnings;
  use parent 'Exporter::Extensible';
  our %EXPORT= ( ... );
  our %EXPORT_TAGS = ( ... );

Everything else below is just convenience and shorthand to make this easier.

=head2 EXPORT BY API

This module provides an api for specifying the exports.  You can call these methods on
C<__PACKAGE__>, or if you ask for version-1 as C<< -export_setup => 1 >> you can use the
convenience function L</export>.

=over

=item export

This function takes a list of keys (which must be scalars), with optional values which must be
refs.  If the value is omitted, C<export> attempts to do-what-you-mean to find it.

=over

=item 'foo' => \&CODE

This declares a normal exported function.  If the ref is omitted, C<export> looks for it in the
hierarchy of the current package.  Note that this lookup happens immediately, and packages
derived from this cannot override C<foo> and have that be exported in its place.

=item '$foo' => \$SCALAR, '@foo' => \@ARRAY, '%foo' => \%HASH, '*foo' => \*GLOB

This exports a normal variable or typeglob.  If the ref is omitted, C<export> looks for it
in the current package.

=item '-foo' => $CODEREF or '-foo' => \"methodname"

This differs from a normal exported function in that it will execute the coderef at import time,
and sub-packages B<can> override it, since it gets called as a method.  The default is to derive
the method name by removing the C<->.

=item ':foo' => \@LIST

Declaring a tag is nothing special; just give it an arrayref of what should be imported when
the tag is encountered.

=item '=$foo' => $CODEREF or '=$foo' => \"methodname"

Prefixing an export name with an equal sign means you want to generate the export on the fly.
The ref is understood to be the coderef or method name to call (as a method) which will return
the ref of the correct type to be exported.  The default is to look for C<_generate_foo>,
C<_generateSCALAR_foo>, C<_generateARRAY_foo>, C<_generateHASH_foo>, etc.

=back

=item exporter_register_symbol

  __PACKAGE__->exporter_register_symbol($name_with_sigil, $ref);

=item exporter_register_option

  __PACKAGE__->exporter_register_option($name, $method, $arg_count);

This declares an "option" like C<-foo>.  The name should B<not> include the leading C<->.
The C<$method> argument can either be a package method name, or a coderef.  The C<$arg_count>
is the number of options to consume from the C<import(...)> list following the option.

To declare an option that consumes a variable number of arguments, specify C<*> for the count
and then write your method so that it returns the number of arguments it consumed.

=item exporter_register_generator

  __PACKAGE__->exporter_register_generator($name_with_sigil, $method);

This declares that you want to generate C<$name_with_sigil> on demand, using C<$method>.
C<$name_with_sigil> may be a tag like C<':foo'>.
C<$method> can be either a coderef or method name.  The function will be called as a method
on an instance of your package.  The instance is the blessed hash of options passed by the
current consumer of your module.

=item exporter_register_tag_members

  __PACKAGE__->exporter_register_tag_members($tag_name, @members);

This pushes a list of C<@members> onto the end of the named tag.  C<$tag_name> should not
include the leading ':'.  These C<@members> are cumulative with tags inherited from parent
packages.  To avoid inheriting tag members, register a generator for the tag, instead.

=back

=head2 EXPORT BY ATTRIBUTES

Attributes are fun.  If you enjoy artistic code, you might like to declare your exports like so:

  sub foo : Export( :foo ) {}
  sub bar : Export(-) {}
  sub _generate_baz : Export(= :foo) {}

instead of

  export( 'foo', '-bar', '=baz', ':foo' => [ 'foo','baz' ] );

The notations supported in the C<Export> attribute are different but similar to those in the
L<export> function.

=over

=item 'foo'

This indicates the export-name of a sub.  A sub may be exported as more than one name.
Note that the first name in the list becomes the official name (superceeding the actual name of
the sub) which will be added to any tags you listed.

=item ':foo'

This requests that the export-name get added to the named tag.  You may specify any number of
tags.

=item '-', '-(N)', '-foo', '-foo(N)'

This sets up the sub as an option, capturing N arguments.  In the cases without a name, the
name of the sub is used.

=item '=', '=$', '=@', '=%', '=*', '=foo', '=$foo', ...

This sets up the sub as a generator for the export-name.  If the word portion of the name is
omitted, it is taken to be the sub name minus the prefix "_generate_" or "_generate$REFTYPE_".

=back

=head2 EXPORT BY VARIABLES

As shown above, the configuration for your exports is the variable C<%EXPORT>.
If you want the fastest possible module load time, you might decide to
populate C<%EXPORT> manually.

The keys of this hash are the strings that the user would specify as the C<import> arguments,
like C<'-foo'>, C<'$foo'>, etc.  The value should be some kind of reference matching the sigil.
Functions should be a coderef, scalars should be a scalarref, etc.  But, there are two special
cases:

=over

=item Options

An option is any key starting with C<->, like this module's own C<-exporter_setup>.  The values
for these must be a pair of C<< [ $method_name, $arg_count_or_star ] >>.  (the default
structure is subject to change, but this notation will always be supported)

  { '-exporter_setup' => [ "exporter_setup", 1 ] }

This means "call C<< $self->exporter_setup($arg1) >> when you see C<< import('-exporter_setup', $arg1, ... ) >>.
Because it is a method call, subclasses of your module can override it.

=item Generators

Sometimes you want to generate the thing to be exported.  To indicate this, use a ref-ref of
the method name, or a ref of the coderef to execute.  For example:

  {
    foo => \\"_generate_foo",
    bar => \\&generate_bar,
    baz => \sub { ... },
  }

Again, this is subject to change, but these notations will always be supported for
backward-compatibility.

=back

=head1 SEE ALSO

Exporter::Tiny

Export::Declare

Badger::Exporter

Sub::Exporter
