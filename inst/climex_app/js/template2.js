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
	'controls': [ 'first', 'play', 'last' ],
	'loopMode': '%loop'
    });
    $('#animationScSh').scianimator({
	'images': [%imgScSh],
	'width': %width,
	'delay': %delay,
	'controls': 'NONE',
	'loopMode': '%loop'
    });
    $('#animationLocSc').scianimator('play');    
    $('#animationLocSh').scianimator('play');
    $('#animationScSh').scianimator('play');

    $('#animationLocSh-play').bind( 'click', function( event ){
	if ( $('#animationLocSh-play').attr( 'class' ).match( 'play' ) ){
	    $('#animationLocSc').scianimator( 'play' );
	    $('#animationScSh').scianimator( 'play' );
	} else {
	    $('#animationLocSc').scianimator( 'stop' );
	    $('#animationScSh').scianimator( 'stop' );
	}
    } );
})(jQuery);
