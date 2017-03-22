$(document).on('shiny:connected', function(event) {
	$(document).on('shiny:inputchanged', function(event){
		if(event.name === 'link_to_exploredata'){
			document.title = "Explore Data";
			console.log("title changed");
		}
		console.log("triggered");
	}

}