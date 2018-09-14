#! /usr/bin/env perl
use strict;
use warnings;
use Test::More;

use_ok( 'Exporter::Extensible' ) or BAIL_OUT;

ok( eval q|
	package Example;
	$INC{'Example.pm'}=1;

	use Exporter::Extensible -exporter_setup => 0;
	sub foo :Export {}
	sub bar :Export {}
|, 'declare Example' ) or diag $@;

ok( eval q|
	package Example::Derived;
	$INC{'Example/Derived.pm'}=1;

	use Example -exporter_setup => 0;
	sub foo :Export {}
|, 'declare Example::Derived' ) or diag $@;

ok( !exists &foo, 'foo not imported yet' );
Example->import('foo');
ok( exists &foo, 'foo imported' );
is( \&foo, \&Example::foo, 'from package Example' );

undef *foo;

Example::Derived->import('foo');
is( \&foo, \&Example::Derived::foo, 'now from package Example::Derived' );

Example::Derived->import('bar');
is( \&bar, \&Example::bar, 'Example::Derived inherits parent\'s "bar"' );

done_testing;
