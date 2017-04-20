(function($) {
    $('#animationLocSc').scianimator({
	'images': [%imgLocSc],
	'width': %width,
	'delay': %delay,
	'controls': 'NONE',
	'loopMode': '%loop'
    });
    $('#animationLocSh').scianimator({
	'images': [%imgLocSh],
	'width': %width,
	'delay': %delay,
	'controls': %initControl,
	'loopMode': '%loop'
    });
    $('#animationScSh').scianimator({
	'images': [%imgScSh],
	'width': %width,
	'delay': %delay,
	'controls': 'NONE',
	'loopMode': '%loop'
    });
    // on start all of the animation should be running
    $('#animationLocSc').scianimator(%initStatus);
    $('#animationLocSh').scianimator(%initStatus);
    $('#animationScSh').scianimator(%initStatus);

    // I just want to have one navigation tool. So its clicking should effect all three objects
    $('#animationLocSh-play').bind( 'click', function( event ){
	if ( $('#animationLocSh-play').attr( 'class' ).match( 'play' ) ){
	    $('#animationLocSc').scianimator( 'play' );
	    $('#animationScSh').scianimator( 'play' );
	} else {
	    $('#animationLocSc').scianimator( 'stop' );
	    $('#animationScSh').scianimator( 'stop' );
	}
    } );
    $( '#animationLocSh-first').bind( 'click', function( event ){
	$('#animationLocSc').scianimator( 'first' );
	$('#animationScSh').scianimator( 'first' );
    });
    $( '#animationLocSh-last').bind( 'click', function( event ){
	$('#animationLocSc').scianimator( 'last' );
	$('#animationScSh').scianimator( 'last' );
    });
    
})(jQuery);
