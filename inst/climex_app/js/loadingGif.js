(function($) {
    /* if the animation is started, show the Gif */
    $('#buttonDrawAnimation').bind( 'click', function( event ){
	$( '#loadingGif' ).css( 'visibility', 'visible' );
	$( '#loadingGif' ).css( 'width', '100%' );
    });
    /* if the animation is started, show the Gif */
    $('#buttonDrawMarkers').bind( 'click', function( event ){
	$( '#loadingGif' ).css( 'visibility', 'visible' );
	$( '#loadingGif' ).css( 'width', '0%' );
    });
    /* since the shiny server should be idle after sending the pictures hide it*/
    $(document).on( 'shiny:idle', function( event ){
	$( '#loadingGif' ).css( 'visibility', 'hidden' );
	$( '#loadingGif' ).css( 'width', '0%' );
    });
})(jQuery);
