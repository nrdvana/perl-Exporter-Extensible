use strict;
use warnings;
require MRO::Compat if "$]" < '5.009005';

# The main problem solved here is that perl earlier than 5.12 does not install
# a sub into the package stash until after calling MODIFY_CODE_ATTRIBUTES, so
# the :Export attributes can't resolve to a name until later.

# There isn't any good spot in the API of Exporter::Extensible to put this
# delayed processing, so about the only way to fix is to just perform it
# before any public API method, and at the end of the scope.

my $process_attr= \&Exporter::Extensible::_exporter_process_attribute;
@Exporter::Extensible::Compat::_pending_attr= ();
{
	no warnings 'redefine';
	*Exporter::Extensible::_exporter_process_attribute= *Exporter::Extensible::Compat::_exporter_process_attribute;
	# Other methods that might be called before end of scope
	for (qw(
		FETCH_CODE_ATTRIBUTES MODIFY_CODE_ATTRIBUTES import exporter_setup
		exporter_export exporter_register_tag_members exporter_register_generator
		exporter_register_option exporter_get_inherited exporter_also_import
		exporter_autoload_tag exporter_get_tag exporter_autoload_symbol
		exporter_register_symbol
	)) {
		my $method= Exporter::Extensible->can($_);
		eval 'sub Exporter::Extensible::'.$_.' {
				Exporter::Extensible::Compat::_process_pending_attrs()
					if @Exporter::Extensible::Compat::_pending_attr;
				goto $method;
			}
			1'
			or die $@;
	};
}

sub Exporter::Extensible::Compat::_queue_attr {
	my ($class, $coderef, $attr)= @_;
	if (!@Exporter::Extensible::Compat::_pending_attr) {
		require B::Hooks::EndOfScope;
		B::Hooks::EndOfScope::on_scope_end(\&Exporter::Extensible::Compat::_process_pending_attrs);
	}
	push @Exporter::Extensible::Compat::_pending_attr, [ @_ ];
}

sub Exporter::Extensible::Compat::_process_pending_attrs {
	while (my $call= shift @Exporter::Extensible::Compat::_pending_attr) {
		$process_attr->(@$call);
	}
}

sub Exporter::Extensible::Compat::_exporter_process_attribute {
	my $ret;
	if (eval { $ret= $process_attr->(@_); 1 }) {
		return $ret;
	}
	elsif ($@ =~ /determine export name/) {
		Exporter::Extensible::Compat::_queue_attr(@_);
		return 1;
	}
	else {
		die $@;
	}
}

1;