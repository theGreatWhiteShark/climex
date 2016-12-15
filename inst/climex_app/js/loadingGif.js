(function($) {
    /* if the animation is started, show the Gif */
    $('#buttonDrawAnimation').bind( 'click', function( event ){
	$( '#loadingGif' ).css( 'visibility', 'visible' );
	$( '#loadingGif' ).css( 'width', '50%' );
    });
    /* if the animation is started, show the Gif */
    $('#buttonDrawMarkers').bind( 'click', function( event ){
	$( '#loadingGif' ).css( 'visibility', 'visible' );
	$( '#loadingGif' ).css( 'width', '50%' );
    });
    /* since the shiny server should be idle after sending the pictures hide it*/
    $(document).on( 'shiny:idle', function( event ){
	$( '#loadingGif' ).css( 'visibility', 'hidden' );
	$( '#loadingGif' ).css( 'width', '0%' );
	
	/* I want to display the leaflet legend  horizontally */
	/* So whenever shiny has nothing to do check if the legend was already created */
	// if ( $( ".leaflet-right > .legend > div > span" ).length ) {
	//     gradientString = $( ".leaflet-right > .legend > div > span" ).css( "background-image" );
	//     gradientString2 = gradientString.replace( "gradient(", "gradient( to right," );
	//     console.log( "1" );
	//     $( ".leaflet-right > .legend > div > span" ).css( "background-image", gradientString2 );
	// };
    });
    /* since the shiny server should be idle after sending the pictures hide it*/
    $(document).on( 'shiny:visualchange', function( event ){
    	/* I want to display the leaflet legend  horizontally */
    	/* So whenever shiny has nothing to do check if the legend was already created */
    	console.log( "shiny:visualchange" );
    	if ( $( ".leaflet-right > .legend > div > span" ).length ) {
    	    gradientString = $( ".leaflet-right > .legend > div > span" ).css( "background-image" );
    	    gradientString2 = gradientString.replace( "gradient(", "gradient( to right," );
    	    console.log( "shiny:visualchange2" );
    	    $( ".leaflet-right > .legend > div > span" ).delay(4000).css( "background-image", gradientString2 );
    	};
    });
    $(document).on( "shiny:idle", function( event ){
    	/* I want to display the leaflet legend  horizontally */
    	/* So whenever shiny has nothing to do check if the legend was already created */
    	console.log( "shiny:idle1" );
    	if ( $( ".leaflet-right > .legend > div > span" ).length ) {
    	    gradientString = $( ".leaflet-right > .legend > div > span" ).css( "background-image" );
    	    gradientString2 = gradientString.replace( "gradient(", "gradient( to right," );
    	    console.log( "shiny:idle2" );
    	    $( ".leaflet-right > .legend > div > span" ).delay(4000).css( "background-image", gradientString2 );
    	    console.log( "shiny:idle3" );
    	};
    });
})(jQuery);
