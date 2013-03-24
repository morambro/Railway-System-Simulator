(function() {
		Raphael.fn.addGuides = function() {
			this.ca.guide = function(g) {
				return {
					guide: g
				};
			};
			this.ca.along = function(percent) {
				var g = this.attr("guide");
				var len = g.getTotalLength();
				var point = g.getPointAtLength(percent * len);
			
				var t = {
					transform: "t" + point.x + " " + point.y
				};
				return t;
			};
		};
	})();
	
var Animations = {

	createRaphael : function(with_guides,parameters) {
		r = new Raphael(parameters.container_id,parameters.width,parameters.height);
		r.addGuides();
		return r;
	},

	runElementOnPath : function(element,path,time,from,to) {
		element.attr(
			{
				guide : path,
				along : from
			}
		).animate(
			{
				along : to
			}, 
			time,
			"ease-out"
		);
	},
	
	createStraightPath : function(canvas,from_x,from_y,to_x,to_y) {
		return canvas.path("M"+from_x+" "+from_y+"L"+to_x+" "+to_y);
	},
	
	translate : function(element, dx, dy) {
		element.attr({
			transform : "t" + dx + " " + dy
		});
	}
	
}

