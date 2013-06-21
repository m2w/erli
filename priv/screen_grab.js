#! /usr/bin/env phantomjs

var page = require('webpage').create(),
system = require('system'),
address, output;

if (system.args.length !== 3) {
    phantom.exit(1);
} else {
    address = system.args[1];
    output = system.args[2];
    page.viewportSize = { width: 800, height: 600 };
    page.open(address, function (status) {
	if (status !== 'success') {
	    phantom.exit();
	} else {
	    window.setTimeout(function () {
		page.render(output);
		phantom.exit();
	    }, 200);
	}
    });
}
