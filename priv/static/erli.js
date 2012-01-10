/* global vars */
var re = /^(?:[a-zA-Z0-9]+:\/\/)(?:(?:(?:[a-zA-Z0-9]+\.)+(?:[a-zA-Z]+))|(?:(?:[0-9]+\.){3}(?:[0-9]+))|(?:(?:[a-f0-9]+\:)+(?:[a-f0-9]+)))(?:(?:\s*$)|(?:(?:[:\/?]).+$))/;

/* utility functions */
function show_error_and_helptext(E) {
    E.parent().addClass("error");
    E.siblings('.help-inline').show();
}

function hide_error_and_helptext(E) {
    E.parent().removeClass("error");
    E.siblings('.help-inline').hide();
}

$(document).ready(function() {
    $(".alert-message.warning").remove(); // close the banner for non-js enabled visitors
    $('.alert-message.error').alert();
    $('#url_structure_popup').popover();
    $('#path_help_popup').popover();

    /* Modal ToU button click handlers */
    $(".modal-footer a.primary").click(function(e){
	$("#tou_checkbox").attr("checked", "checked");
	$("#modal-tou").modal("hide");
	e.preventDefault();
    });
    $(".modal-footer a.secondary").click(function(e){
	$("#modal-tou").modal("hide");
	e.preventDefault();
    });

    /* validation functions for the URL submission form */
    $('#tou_checkbox').change(function(e){
	var parDiv = $(this).parents('div.clearfix.input');
	parDiv.removeClass("error");
    });
    $('#url_input').keyup(function(){
	hide_error_and_helptext($(this));
    });
    $('#pref_path_input').keyup(function() {
	$.ajax({
	    type: 'GET',
	    url: $(this).val() + '/check', // need to append the check call to guarantee that the status code is either 200 or 404
	    statusCode:
	    {
		404: function() {
		    // path available
		    hide_error_and_helptext($('#pref_path_input'));
		},
		200: function() {
		    // path taken
		    show_error_and_helptext($('#pref_path_input'));
		}
	    }
	});
    });

    $('#url_input').blur(function(){
	if (!$('#url_input').val().match(re)){
	    show_error_and_helptext($('#url_input'));
	}
    });

    /* form submission */
    $("#shorten_url_form").submit(function() {
	var url = $("#url_input").val();
	var checked = $("#tou_checkbox").attr("checked") ? true : false;
	var pref_path = $('#pref_path_input').val();
	if (!checked || !url) {
	    if (!url) {
		show_error_and_helptext($('#url_input'));
	    }
	    if (!checked) {
		$('#tou_checkbox').parents('div.clearfix.input').addClass("error");
	    }
	    return false;
	}

	if (!url.match(re)){
	    show_error_and_helptext($('#url_input'));
	    return false;
	}
	if (pref_path) {
	    $.ajax({
		type: 'PUT',
		url: pref_path,
		contentType: 'application/json',
		data: JSON.stringify({
		    url: url,
		    tou_checked: checked
		}),
		statusCode: {
		    204: function(){
			$('#path').attr('href', pref_path).text(pref_path);
			$('#path_stats').attr('href', pref_path + '/stats').text(pref_path + '/stats');
			$('#success_banner').show();
		    },
		    400: function(){
			// bad request - should never occur
		    },
		    409: function(){
			// conflict, pref_path is already taken
			show_error_and_helptext($('#pref_path_input'));
		    },
		    410: function(){
			// target url is permanently banned
			$('#banned_url').text($('#url_input').val());
			$('.alert-message.error').show();
		    }
		}
	    });
	}
	else {
	    $.ajax({
		type:'POST',
		url:'/',
		data: JSON.stringify({
		    url: url,
		    tou_checked: checked
		}),
		contentType: 'application/json',
		dataType: 'json',
		processData: false, // we want the raw json object to be transmitted
		statusCode: {
		    201: function(data, textStatus, jqXHR) {
			var location = jqXHR.getResponseHeader('Location');
			$('#path').attr('href', location).text(location);
			$('#path_stats').attr('href', location + '/stats').text(location + '/stats');
			$('#success_banner').show();
		    },
		    400: function(){
		    },
		    410: function(){
			$('#banned_url').text($('#url_input').val());
			$('.alert-message.error').show();
		    },
		    500: function(){
		    }
		}
	    });
	}
	return false;
    });
});
