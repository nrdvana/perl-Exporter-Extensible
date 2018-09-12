#! /usr/bin/env perl
use strict;
use warnings;
use Test::More;

use_ok( 'Exporter::Extensible' ) or BAIL_OUT;

package Example;

use Exporter::Extensible -parent;
sub foo :Export {
	print "foo\n";
}

package main;

ok( !exists &foo, 'foo not imported yet' );
Example->import('foo');
ok( exists &foo, 'foo imported' );

done_testing;
