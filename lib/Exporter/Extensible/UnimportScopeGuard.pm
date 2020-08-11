package Exporter::Extensible::UnimportScopeGuard;
require Exporter::Extensible; # comtains implementation of UnimportScopeGuard

# ABSTRACT: Unimport set of symbols when object is destroyed

=head1 DESCRIPTION

This is an implementation detail of how Exporter::Extensible removes symbols at the
end of a scope.  The DESTROY method of this package performs the removal of the symbols.

=cut
