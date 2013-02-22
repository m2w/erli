/* global vars */
var re = /^(?:[a-zA-Z0-9]+:\/\/)(?:(?:(?:[a-zA-Z0-9]+\.)+(?:[a-zA-Z]+))|(?:(?:[0-9]+\.){3}(?:[0-9]+))|(?:(?:[a-f0-9]+\:)+(?:[a-f0-9]+)))(?:(?:\s*$)|(?:(?:[:\/?]).+$))/;

/* utility functions */
function reset_form(){
	remove_highlights_and_helptext($('#input_pref_shortened_url')).val('');
	remove_highlights_and_helptext($('#input_target_url')).val('');
}
function remove_banners() {
	$('#banner_url_shortened').hide();
	$('#banner_target_url_banned').hide();
}
function show_error_and_helptext(E) {
    E.parent().removeClass('success').addClass('error');
    E.siblings('.help-inline').show();
    E.siblings('small').children().show()
    return E;
}
function remove_highlights_and_helptext(E) {
    E.parent().removeClass('error').removeClass('success');
    E.siblings('.help-inline').hide();
    E.siblings('small').children().hide()
    return E;
}
function validate_target_url() {
	if ($('#input_target_url').val() && !$('#input_target_url').val().match(re)){
	    show_error_and_helptext($('#input_target_url'));
	}
}
function show_pref_url_not_available() {
	$('#input_pref_shortened_url').siblings('small').children().text('Unfortunately that shortened URL is already in use');
    show_error_and_helptext($('#input_pref_shortened_url'));
}

$(document).ready(function() {
	/* setup */
    $('#banner_js_disabled').remove(); // close the banner for non-js enabled visitors
    $('#banner_target_url_banned .close').click(function(e){$('#banner_target_url_banned').hide()});
    $('#banner_url_shortened .close').click(function(e){$('#banner_url_shortened').hide()});
    $('#popover_schema_required').popover({placement: 'bottom', trigger: 'hover'});

    /* Modal ToU button click handlers */
    $('.modal-footer a.btn-success').click(function(e){
		$('#input_tou_check').attr('checked', 'checked');
		$('#modal_tou').modal('hide');
		e.preventDefault();
    });
    $('.modal-footer a.btn-inverse').click(function(e){
		$('#modal_tou').modal('hide');
		e.preventDefault();
    });

	/* functions to handle user/form interaction */
    $('#btn_check_availability').click(function(e){
    	var input = $('#input_pref_shortened_url').val();
		$.ajax({
		    type: 'GET',
		    url: input + '/check', // need to append the check call to guarantee that the status code is either 200 or 404
		    statusCode:
		    {
				404: function() {
				    // path available
				    remove_highlights_and_helptext($('#input_pref_shortened_url'));
				    $('#input_pref_shortened_url').parent().addClass('success');

				},
				200: function() {
				    // path taken
				    remove_highlights_and_helptext($('#input_pref_shortened_url'));
				    show_pref_url_not_available();
				}
		    }
		});
    	e.preventDefault();
    });
    $('#input_tou_check').change(function(e){
		$(this).parent().removeClass('error');
    });
    $('#input_target_url').keyup(function(){
		remove_highlights_and_helptext($(this));
    });

    $('#input_target_url').blur(function(){
    	remove_highlights_and_helptext($(this));
    	validate_target_url();
    });

    /* form submission */
    $('#form_shorten_url').submit(function() {
		var url = $('#input_target_url').val();
		var checked = $('#input_tou_check').is(':checked') ? true : false;
		var pref_path = $('#input_pref_shortened_url').val().trim();
		if (!checked || !url) {
		    if (!url) {
				show_error_and_helptext($('#input_target_url'));
		    }
		    if (!checked) {
				$('#input_tou_check').parent().addClass('error');
		    }
		    return false;
		}

		if (!url.match(re)){
		    show_error_and_helptext($('#input_target_url'));
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
						$('#shortened_url').attr('href', pref_path).text(pref_path);
						$('#url_to_stats').attr('href', pref_path + '/stats').text(pref_path + '/stats');
						$('#banner_url_shortened').show();
						reset_form();
				    },
				    409: function(){
						// conflict, pref_path is already taken
						show_pref_url_not_available();
						show_error_and_helptext($('#input_pref_shortened_url'));
				    },
				    410: function(){
						// target url is permanently banned
						$('#banned_url').text(url);
						$('#banner_target_url_banned').show();
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
				    201: function(E) {
						var location = E.getResponseHeader('Location');
						$('#shortened_url').attr('href', location).text(location);
						$('#url_to_stats').attr('href', location + '/stats').text(location + '/stats');
						$('#banner_url_shortened').show();
						reset_form();
				    },
				    410: function(){
						// target url is permanently banned
						$('#banned_url').text(url);
						$('#banner_target_url_banned').show();
				    },
				    500: function(){
						// internal server error, occurs when generation of a random short URL fails unexpectedly
				    	$('#banner_internal_error').show();
				    }
				}
		    });
		}
		return false;
    });
});
