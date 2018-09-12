package Exporter::Extensible;

use strict;
use warnings;

our %EXPORT_SYMS_PKG_CACHE;
our %EXPORT_GENS_PKG_CACHE;
our %EXPORT_TAGS_PKG_CACHE;

our %EXPORT_SYMS= (
	-exporter_setup_subclass => [ 'exporter_setup_subclass', 0 ],
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
	my $export_sym= ($EXPORT_SYMS_PKG_CACHE{ref $self} ||= {});
	my $export_gen= ($EXPORT_GENS_PKG_CACHE{ref $self} ||= {});
	
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
		my $ref= (
		    exists $export_sym->{$symbol}? $export_sym->{$symbol}
		  : defined $export_gen->{$symbol}? $export_gen->{$symbol}->($self, $symbol)
		  : $self->exporter_get_symbol($symbol)
		) or _croak("'$symbol' is not exported by ".ref($self));
		
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
		no warnings 'uninitialized';
		my $dest= $self->{prefix}.(defined $self->{-as}? $self->{-as} : $name).$self->{suffix};
		if (ref $into eq 'HASH') {
			$into->{$dest}= $symbol;
			next;
		}
		$dest= $into.'::'.$dest;
		if (exists &$dest and ref $symbol eq 'CODE' and \&$dest != $symbol) {
			my $r= $self->{replace} || $self->{-replace};
			if (!$r || $r eq 'carp' || $r eq 'warn') {
				_carp("Overwriting existing sub '$dest' with sub '$name' exported by ".ref($self));
			}
			elsif ($r eq 'croak' || $r eq 'fatal' || $r eq 'die') {
				_carp("Refusing to overwrite existing sub '$dest' with sub '$name' exported by ".ref($self));
			}
		}
		*$dest= $ref;
		push @{${$self->{scope}}}, $dest if $self->{scope};
	}
}

sub exporter_register_symbol {
	my ($class, $export_name, $ref)= @_;
	$class= ref($class)||$class;
	no strict 'refs';
	${$class.'::EXPORT_SYMS'}{$export_name}= $ref;
}

sub exporter_get_symbol {
	my ($self, $sym)= @_;
	my $class= ref($self)||$self;
	# Make the common case fast.
	return $EXPORT_SYMS_PKG_CACHE{$class}{$sym}
		if exists $EXPORT_SYMS_PKG_CACHE{$class}{$sym};
	return $EXPORT_GENS_PKG_CACHE{$class}{$sym}->($self, $sym)
		if defined $EXPORT_GENS_PKG_CACHE{$class}{$sym};
	# search package hierarchy
	my $hier= mro::get_linear_isa($class);
	no strict 'refs';
	for (@$hier) {
		if (exists ${$_.'::EXPORT_SYMS'}{$sym}) {
			my $ref= ${$_.'::EXPORT_SYMS'}{$sym};
			# If this is a plain method of the parent class, then possibly upgrade it to a subclassed method
			$ref= $self->can($sym) if ref($ref) eq 'CODE' && $ref == $_->can($sym);
			return ($EXPORT_SYMS_PKG_CACHE{$class}{$sym}= $ref);
		}
		return ($EXPORT_GENS_PKG_CACHE{$class}{$sym}= ${$_.'::EXPORT_GENS'}{$sym})->($self, $sym)
			if defined ${$_.'::EXPORT_GENS'}{$sym};
	}
	# Isn't exported.
	return undef;
}

sub exporter_register_option {
	my ($class, $option_name, $method_name, $arg_count)= @_;
	$class= ref($class)||$class;
	no strict 'refs';
	${$class.'::EXPORT_SYMS'}{'-'.$option_name}= [ $method_name, $arg_count||0 ];
}

sub exporter_register_generator {
	my ($class, $export_name, $method_name)= @_;
	$class= ref($class)||$class;
	no strict 'refs';
	${$class.'::EXPORT_GENS'}{$export_name}= $method_name;
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
		# Collect all members of this tag from any parent class
		my $hier= mro::get_linear_isa($class);
		no strict 'refs';
		my %seen;
		for (@$hier) {
			my $tag= ${$_.'::EXPORT_TAGS'}{$tagname} or next;
			$seen{$_}++ for @$tag;
		}
		$EXPORT_TAGS_PKG_CACHE{$class}{$tagname}= $list= [ keys %seen ];
	}
	# Apply "not" exclusions
	if (ref $self && (my $not= $self->{not} || $self->{-not})) {
		$list= [ @$list ]; # clone before removing exclusions
		# N^2 exclusion iteration isn't cool, but doing something smarter requires a
		# lot more setup that probably won't pay off for the usual tiny lists of 'not'.
		for my $filter (ref $not eq 'ARRAY'? @$not : ($not)) {
			if (ref $filter eq 'RegExp') {
				@$list= grep $_ !~ $filter, @$list;
			} else {
				@$list= grep $_ ne $filter, @$list;
			}
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
sub _exporter_process_attribute {
	my ($class, $coderef, $attr)= @_;
	if ($attr =~ /^Export(\(.*?\))?$/) {
		my (%tags, $subname, $export_name);
		# If given a list in parenthesees, split on space and proces each.  Else use the name of the sub itself.
		for my $token ($1? split(/\s+/, substr($1, 1, -1)) : ()) {
			if ($token =~ /^:(.*)$/) {
				$tags{$1}++; # save tags until we have the export_name
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
			elsif ($token =~ /^\w+$/) {
				$export_name ||= $token;
				$class->exporter_register_symbol($token, $coderef);
			}
			else {
				_croak("Invalid export notation '$token'");
			}
		}
		if (!defined $export_name) { # if list was empty or only tags...
			$export_name= _exporter_get_coderef_name($coderef);
			$class->exporter_register_symbol($export_name, $coderef);
		}
		$class->exporter_register_tag_members($_, $export_name) for keys %tags;
		return 1;
	}
	return;
}

sub exporter_setup_subclass {
	my $self= shift;
	no strict 'refs';
	push @{$self->{into}.'::ISA'}, ref($self);
	strict->import;
	warnings->import;
}

1;

=head1 DESCRIPTION

As a module author, you have lots of exporters to choose from, so I'll try to get straight to
the pros/cons of this module:

=head2 Pros

=over

=item Extend Your Module

This module focuses on the ability and ease of letting you "subclass" a module-with-exports to
create a derived module-with-exports.

=item Extend Behavior of C<import>

This module supports lots of ways to add custom processing during 'import' without needing to
dig into the implementation too much.

=item Advanced Features

This module attempts to copy useful features from other Exporters, like renaming imports per
-consumer, prefixes, suffixes, excluding symbols, importing to things other than C<caller>,
declaring imports with an API, or with attributes, etc.

=item No Non-core Dependencies

Because nobody likes extra deps forced into them.

=item Speed

(it hasn't been thoroughly optimized yet, but it was designed with low overhead in mind)

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

=item (Small) Namespace and inheritance pollution

This module defines a few API methods used for the declaration and definition of the export
process.  Exporting modules must inherit from this module, thus inheriting that API as well.
The API I<doesn't> get exported to consumers even if they request ':all', so it's only a small
worry for name collisions in the packages defining the exports.

I could have kept these meta-methods in a separate namespace, but that would defeat the goal of
"easy to extend".

If you want a pure class hierarchy for OO purposes but also export a few symbols, consider
making a separate module for the exported symbols and then add this to your class:

    package My::Class;
    require My::Class::Exports;
    sub import { My::Module::Exports->import_into(scalar caller, @_) }

=back

=head1 SEE ALSO

Exporter::Tiny

Export::Declare

Badger::Exporter

Sub::Exporter
