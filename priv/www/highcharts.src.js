// ==ClosureCompiler==
// @compilation_level SIMPLE_OPTIMIZATIONS
// ==/ClosureCompiler==
/** 
 * @license Name:    Highcharts
 * Version: 1.0.2 (2009-12-09)
 * Author:  Vevstein Web T.H.
 * Support: www.highcharts.com/support
 * License: www.highcharts.com/license
 */

/* 
 * Next version
 * - dual axis
 * - point object with prototype and methods for the most common point operations
 * 		that are now done in loops in the series prototypes.
 * - skip excanvas, attach drawing methods to the layer itself?
 * - animations: type definitions, easing, duration, common morphing, chaining
 * - use mouseenter and mouseleave for imagemap
 * - events: select, series.click, point.click, series.legendClick? ....
 */

(function() {
var 
	// abstracts to make compiled code smaller
	undefined,
	doc = document,
	win = window,
	math = Math,
	mathRound = math.round,
	mathFloor = math.floor,
	mathAbs = math.abs,
	mathCos = math.cos,
	mathSin = math.sin,	
	
	
	// some variables
	userAgent = navigator.userAgent,
	isIE = /msie/i.test(userAgent) && !win.opera,
	isWebKit = /AppleWebKit/.test(userAgent) ,
	canvasCounter = 0,
	colorCounter,
	symbolCounter,
	symbolSizes = {},
	idCounter = 0,
	timeFactor = 1, // 1 = JavaScript time, 1000 = Unix time
	
	// some constants for frequently used strings
	DIV = 'div',
	ABSOLUTE = 'absolute',
	RELATIVE = 'relative',
	HIDDEN = 'hidden',
	VISIBLE = 'visible',
	PX = 'px',
	
/**
 * Utility functions
 */
	each,
	map,
	merge,
	hyphenate,
	addEvent,
	animate,
	getAjax;
	
if (win.jQuery) {
	var jQ = jQuery;
	each = function(arr, fn){
		for (var i = 0, len = arr.length; i < len; i++)
			if (fn.call(arr[i], arr[i], i, arr) === false) return i;
	}
	map = function(arr, fn){
		//return jQuery.map(arr, fn);
		var results = [];
		for (var i = 0, len = arr.length; i < len; i++) 
			results[i] = fn.call(arr[i], arr[i], i, arr);
		return results;
		
	}
	merge = function(){
		var args = arguments;
		return jQ.extend(true, null, args[0], args[1], args[2], args[3]);
	}
	hyphenate = function (str){
		return str.replace(/([A-Z])/g, function(a, b){ return '-'+ b.toLowerCase() });
	}
	addEvent = function (el, event, fn){
		jQ(el).bind(event, fn);
	}
	jQ.extend( jQ.easing, {
		easeOutQuad: function (x, t, b, c, d) {
			return -c *(t/=d)*(t-2) + b;
		}
	});

	animate = function (el, params, options){	
		jQ(el).animate(params, options);
	}
	getAjax = function (url, callback) {
		jQ.get(url, null, callback);
	}
	
} else if (win.MooTools) {
	each = function (arr, fn){
		arr.each(fn); // Mootools specific
	}
	map = function (arr, fn){
		return arr.map(fn);
	}
	merge = function (){
		if (win.$merge) return $merge.apply(this, arguments);
		
	}
	hyphenate = function (str){
		return str.hyphenate();
	}
	addEvent = function (el, event, fn){
		el.addEvent(event, fn);
	}
	animate = function (el, params, options){
		var myEffect = new Fx.Morph($(el), extend(options, {
	 		transition: Fx.Transitions.Quad.easeInOut
	 	}));
		myEffect.start(params);
	}
	getAjax = function (url, callback) {
		(new Request({
			url: url,
			method: 'get',
			onSuccess: callback
		})).send();			
	}
	
} 


function extend(a, b) {
	if (!a) a = {};
	for (var n in b) a[n] = b[n];
	return a;
}

/**
 * Merge the default options with custom options and return the new options structure
 * @param {Object} options The new custom options
 */
function setOptions(options) {
	return defaultOptions = merge(defaultOptions, options);
}

var defaultFont = 'normal 12px "Lucida Grande", "Lucida Sans Unicode", Verdana, Arial, Helvetica, sans-serif',

defaultLabelOptions = {
	enabled: true,
	// rotation: 0,
	align: 'center',
	x: 0,
	y: 15,
	/*formatter: function() {
		return this.value;
	},*/
	style: {
		color: '#666',
		font: defaultFont.replace('12px', '11px') 
		//'10px bold "Lucida Grande", "Lucida Sans Unicode", Verdana, Arial, Helvetica, sans-serif'	
	}
},
defaultOptions = {
	colors: ['#4572A7', '#AA4643', '#89A54E', '#80699B', '#3D96AE', 
		'#DB843D', '#92A8CD', '#A47D7C', '#B5CA92'],
	symbols: ['circle', 'diamond', 'square', 'triangle', 'triangle-down'],
	lang: {
		months: ['January', 'February', 'March', 'April', 'May', 'June', 'July', 
				'August', 'September', 'October', 'November', 'December'],
		weekdays: ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday']
	},
	chart: {
		//className: null,
		margin: [50, 50, 60, 80],
		borderColor: '#4572A7',
		//borderWidth: 0,
		borderRadius: 5,		
		defaultSeriesType: 'line',
		//inverted: false,
		//shadow: false,
		//style: {},
		//backgroundColor: null,
		//plotBackgroundColor: null,
		plotBorderColor: '#C0C0C0'
		//plotBorderWidth: 0,
		//plotShadow: false,
		//zoomType: ''
	},
	title: {
		text: 'Chart title',
		style: {
			textAlign: 'center',
			color: '#3E576F',
			font: defaultFont.replace('12px', '16px'),
			//'16px "Lucida Grande", "Lucida Sans Unicode", Verdana, Arial, Helvetica, sans-serif',
			margin: '10px 0 0 0'
		}

	},
	subtitle: {
		text: '',
		style: {
			textAlign: 'center',
			color: '#6D869F',
			font: defaultFont,
			margin: 0
		}
	},
	xAxis: {
		// alternateGridColor: null,
		//categories: [],
		dateTimeLabelFormats: {
			second: '%H:%M:%S',
			minute: '%H:%M',
			hour: '%H:%M',
			day: '%e. %b',
			week: '%e. %b',
			month: '%b \'%y',
			year: '%Y'
		},
		gridLineColor: '#C0C0C0',
		// gridLineWidth: 0,
		// reversed: false,
		
		labels: defaultLabelOptions,
		lineColor: '#C0D0E0',
		lineWidth: 1,
		max: null,
		min: null,
		maxZoom: 1,
		minorGridLineColor: '#E0E0E0',
		minorGridLineWidth: 1,
		minorTickColor: '#A0A0A0',
		//minorTickInterval: null,
		minorTickLength: 2,
		minorTickPosition: 'outside', // inside or outside
		minorTickWidth: 1,
		//plotBands: [],
		//reversed: false,
		showFirstLabel: true,
		showLastLabel: false,
		startOfWeek: 1, 
		tickColor: '#C0D0E0',
		tickInterval: 'auto',
		tickLength: 5,
		tickmarkPlacement: 'between', // on or between
		tickPixelInterval: 100,
		tickPosition: 'outside',
		tickWidth: 1,
		title: {
			enabled: false,
			text: 'X-values',
			align: 'middle', // low, middle or high
			margin: 35,
			//rotation: 0,
			//side: 'outside',
			style: {
				color: '#6D869F',
				font: defaultFont.replace('normal', 'bold')
			}
		},
		type: 'linear' // linear or datetime
	}, 
	plotOptions: {
		line: { // base series options
	
			lineWidth: 2,
			shadow: true,
			// stacking: null,
			marker: { 
				enabled: true,
				symbol: 'auto',
				lineWidth: 0,
				radius: 4,
				lineColor: '#FFFFFF',
				fillColor: 'auto',
				states: {
					hover: {
						//radius: base + 2
					}							
				}
			},
			dataLabels: merge(defaultLabelOptions, {
				enabled: false,
				y: -6,
				formatter: function() {
					return this.y;
				}
			}),
			
			//pointStart: 0,
			//pointInterval: 1,
			showInLegend: true,
			states: {
				hover: {
					lineWidth: 3,
					marker: {
						// lineWidth: base + 1,
						// radius: base + 1
					}
				}
			}
		}
	},
	labels: {
		//items: [],
		style: {
			position: ABSOLUTE,
			color: '#3E576F',
			font: defaultFont
		}
	},
	legend: {
		enabled: true,
		layout: 'horizontal',
		labelFormatter: function() {
			return this.name
		},
		//borderWidth: 0,
		borderColor: '#909090',
		borderRadius: 5,
		shadow: true,
		//backgroundColor: null,
		style: {
			position: ABSOLUTE,
			zIndex: 10,
			bottom: '10px',
			left: '80px',
			padding: '5px'
		},
		itemStyle: {
			listStyle: 'none',
			margin: '0 1em 0 0',
			padding: 0,
			font: defaultFont,
			cursor: 'pointer',
			color: '#3E576F'
		},
		itemHoverStyle: {
			color: '#000'
		},
		itemHiddenStyle: {
			color: '#CCC'
		},
		symbolWidth: 16,
		symbolPadding: 5
	},
	
	tooltip: {
		enabled: true,
		formatter: function() {
			return '<b>'+ (this.point.name || this.series.name) +'</b><br/>'+
				'X value: '+ this.x +'<br/>'+
				'Y value: '+ this.y;
		},
		backgroundColor: 'rgba(255, 255, 255, .85)',
		borderWidth: 2,
		borderRadius: 5,
		shadow: true,
		style: {
			color: '#333333',
			fontSize: '9pt',
			padding: '5px',
			font: defaultFont
		}
	},
	
	toolbar: {
		itemStyle: {
			color: '#4572A7',
			cursor: 'pointer',
			margin: '20px',
			font: defaultFont
		}
	},
	
	credits: {
		enabled: true,
		text: 'Highcharts.com',
		href: 'http://www.highcharts.com',
		style: {
			position: ABSOLUTE,
			right: '50px',
			bottom: '5px',
			color: '#999',
			textDecoration: 'none',
			font: defaultFont.replace('12px', '10px')
		}
	}
};

// Axis defaults
//defaultOptions.xAxis = merge(defaultOptions.axis);
defaultOptions.yAxis = merge(defaultOptions.xAxis, {
	gridLineWidth: 1,
	tickPixelInterval: 72,
	showLastLabel: true,
	labels: {
		align: 'right',
		x: -8,
		y: 3
	},
	lineWidth: 0,
	rotation: 270, 
	tickWidth: 0,
	title: {
		enabled: true,
		margin: 40,
		rotation: 270,
		text: 'Y-values'
	}
});

// Series defaults
var defaultPlotOptions = defaultOptions.plotOptions, 
	defaultSeriesOptions = defaultPlotOptions.line; 
//defaultPlotOptions.line = merge(defaultSeriesOptions);
defaultPlotOptions.spline = merge(defaultSeriesOptions);
defaultPlotOptions.scatter = merge(defaultSeriesOptions, {
	lineWidth: 0,
	states: {
		hover: {
			lineWidth: 0
		}
	}
});
defaultPlotOptions.area = merge(defaultSeriesOptions, {
	// lineColor: null, // overrides color, but lets fillColor be unaltered
	// fillOpacity: .75,
	fillColor: 'auto'

});
defaultPlotOptions.areaspline = merge(defaultPlotOptions.area);
defaultPlotOptions.column = merge(defaultSeriesOptions, {
	borderColor: '#FFFFFF',
	borderWidth: 1,
	borderRadius: 0,
	groupPadding: .2,
	pointPadding: .1,
	states: {
		hover: {
			brightness: .1,
			shadow: false
		}
	}
});
defaultPlotOptions.bar = merge(defaultPlotOptions.column, {
	dataLabels: {
		align: 'left',
		x: 5,
		y: 0
	}
});
defaultPlotOptions.pie = merge(defaultSeriesOptions, {
	center: ['50%', '50%'],
	legendType: 'point',
	size: '90%',
	slicedOffset: 10,
	states: {
		hover: {
			brightness: .1,
			shadow: false
		}
	}
	
});
var invertedDefaultOptions = {
	xAxis: { // vertical axis
		reversed: true,
		labels: {
			align: 'right',
			x: -8,
			y: 3
		},
		title: {
			rotation: 270
		}
	},
	yAxis: { // horizontal axis
		labels: {
			align: 'center',
			x: 0,
			y: 14
		},
		title: {
			rotation: 0
		}
	}
};

// class-like inheritance
function extendClass(parent, members){
	var object = function(){};
	object.prototype = new parent();
	extend(object.prototype, members);
	return object;
}

function reverseArray(arr) {
	var reversed = [];
	for (var i = arr.length - 1; i >= 0; i--)
		reversed.push( arr[i]);
	return reversed;
}
// return a deep value without throwing an error
function deepStructure(obj, path) {
	// split the path into an array
	path = path.split('.'), i = 0;
	// recursively set obj to the path
	while (path[i] && obj) obj = obj[path[i++]];
	
	if (i == path.length) return obj;
}

/**
 * Create a color from a string or configuration object
 * @param {Object} val
 */
function setColor(val, ctx) {
	if (typeof val == 'string') {
		return val;

	} else if (val.linearGradient) {
		var gradient = ctx.createLinearGradient.apply(ctx, val.linearGradient);
		each (val.stops, function(stop) {
			gradient.addColorStop(stop[0], stop[1]);
		});
		return gradient;
	}
}


var Color = function(input) {
	var rgba, result;
	function parse(input) {
		
		// rgba
		if((result = /rgba\(\s*([0-9]{1,3})\s*,\s*([0-9]{1,3})\s*,\s*([0-9]{1,3})\s*,\s*([0-9]+(?:\.[0-9]+)?)\s*\)/.exec(input)))
			rgba = [parseInt(result[1]), parseInt(result[2]), parseInt(result[3]), parseFloat(result[4])];	

		// hex
		else if((result = /#([a-fA-F0-9]{2})([a-fA-F0-9]{2})([a-fA-F0-9]{2})/.exec(input)))
			rgba = [parseInt(result[1],16), parseInt(result[2],16), parseInt(result[3],16), 1];
	
	}
	function get() {
		if (rgba) return 'rgba('+ rgba.join(',') +')';
		else return input;
	}
	
	function brighten(alpha) {
		if (typeof alpha == 'number' && alpha != 0) {
			for (var i = 0; i < 3; i++) {
				rgba[i] += parseInt(alpha * 255);
				if (rgba[i] < 0) rgba[i] = 0;
				if (rgba[i] > 255) rgba[i] = 255;
			}
		}
		return this;
	}
	
	function setOpacity(alpha) {
		rgba[3] = alpha;
		return this;
	}	
	
	parse(input);
	
	// public methods
	return {
		get: get,
		brighten: brighten,
		setOpacity: setOpacity
	};
};

	//defaultMarkers = ['circle'];


function createElement (tag, attribs, styles, parent, nopad) {
	var el = doc.createElement(tag);
	if (attribs) extend(el, attribs);
	if (nopad) setStyles(el, {padding: 0, border: 'none', margin: 0});
	if (styles) setStyles(el, styles);
	if (parent) parent.appendChild(el);	
	return el;
};

function setStyles (el, styles) {
	//for (var x in styles) el.style[x] = styles[x];
	if (isIE) {
		if (styles.opacity !== undefined) 
			styles.filter = 'alpha(opacity='+ (styles.opacity * 100) +')';
		if (styles.cssFloat !== undefined) styles.styleFloat = styles.cssFloat;		
	}
	extend(el.style, styles);
};
function numberFormat (number, decimals, decPoint, thousandsSep) {
	// http://kevin.vanzonneveld.net/techblog/article/javascript_equivalent_for_phps_number_format/
	var n = number, c = isNaN(decimals = mathAbs(decimals)) ? 2 : decimals,
    	d = decPoint === undefined ? "." : decPoint,
    	t = thousandsSep === undefined ? "," : thousandsSep, s = n < 0 ? "-" : "",
    	i = parseInt(n = mathAbs(+n || 0).toFixed(c)) + "", j = (j = i.length) > 3 ? j % 3 : 0;
    
    return s + (j ? i.substr(0, j) + t : "") + i.substr(j).replace(/(\d{3})(?=\d)/g, "$1" + t) + (c ? d + mathAbs(n - i).toFixed(c).slice(2) : "");
};

/**
 * Based on http://www.php.net/manual/en/function.strftime.php 
 * @param {String} format
 * @param {Number} timestamp
 * @param {Boolean} capitalize
 */
function dateFormat(format, timestamp, capitalize) {
	function pad (number) {
		return number.toString().replace(/^([0-9])$/, '0$1');
	}
	var date = new Date(timestamp * timeFactor),
		hours = date.getUTCHours(),
		day = date.getUTCDay(),
		dayOfMonth = date.getUTCDate(),
		month = date.getUTCMonth(),
		fullYear = date.getUTCFullYear(),
		amPm = 
		
		lang = defaultOptions.lang,
		langWeekdays = lang.weekdays,
		langMonths = lang.months,
		
		// list all format keys
		replacements = {

			// Day
			'a': langWeekdays[day].substr(0, 3), // Short weekday, like 'Mon'
			'A': langWeekdays[day], // Long weekday, like 'Monday'
			'd': pad(dayOfMonth), // Two digit day of the month, 01 to 31 
			'e': dayOfMonth, // Day of the month, 1 through 31 
			
			// Week (none implemented)			
			
			// Month
			'b': langMonths[month].substr(0, 3), // Short month, like 'Jan'
			'B': langMonths[month], // Long month, like 'January'
			'm': pad(month + 1), // Two digit month number, 01 through 12
			
			// Year
			'y': fullYear.toString().substr(2, 2), // Two digits year, like 09 for 2009
			'Y': fullYear, // Four digits year, like 2009
			
			// Time
			'H': pad(hours), // Two digits hours in 24h format, 00 through 23
			'I': pad((hours % 12) || 12), // Two digits hours in 12h format, 00 through 11
			'l': (hours % 12) || 12, // Hours in 12h format, 1 through 11
			'M': pad(date.getUTCMinutes()), // Two digits minutes, 00 through 59
			'p': hours < 12 ? 'AM' : 'PM', // Upper case AM or PM
			'P': hours < 12 ? 'am' : 'pm', // Lower case AM or PM
			'S': pad(date.getUTCSeconds()) // Two digits seconds, 00 through  59
			
		};
		
	// do the replaces
	for (var key in replacements) format = format.replace('%'+ key, replacements[key]);
		
	// Optionally capitalize the string and return
	return capitalize ? format.substr(0, 1).toUpperCase() + format.substr(1) : format;
};

function getPosition (el)	{
	var p = { x: el.offsetLeft, y: el.offsetTop };
	while (el.offsetParent)	{
		el = el.offsetParent;
		p.x += el.offsetLeft;
		p.y += el.offsetTop;
		if (el != doc.body && el != doc.documentElement) {
			p.x -= el.scrollLeft;
			p.y -= el.scrollTop;
		}
	}
	return p;
}

var Layer = function (name, appendTo, props, styles) {
	var layer = this,
		div;
	
	props = extend({
		className: 'highcharts-'+ name
	}, props);
	styles = extend({
		width: appendTo.offsetWidth + PX,
		height: appendTo.offsetHeight + PX,
		position: ABSOLUTE,
		top: 0,
		left: 0,
		margin: 0,
		padding: 0,
		border: 'none'		
	}, styles);
	
	div = createElement(DIV, props, styles, appendTo);
	extend(layer, {
		div: div,
		width: div.offsetWidth,
		height: div.offsetHeight
	});
	
	// initial SVG string
	layer.svg = isIE ? '' : '<?xml version="1.0" encoding="utf-8"?>'+
		'<svg version="1.1" xmlns="http://www.w3.org/2000/svg" '+
		'xmlns:xlink="http://www.w3.org/1999/xlink" width="'+ layer.width 
		+'px" height="'+ layer.height +'">';
			
	
}
Layer.prototype = {
	getCtx: function() {
		if (!this.ctx) {
			var cvs = createElement('canvas', {
				id: 'highcharts-canvas-' + idCounter++,
				width: this.width,
				height: this.height
			}, {
				position: ABSOLUTE
			}, this.div);
			
			if (isIE) {
				G_vmlCanvasManager.initElement(cvs);
				cvs = doc.getElementById(cvs.id);
			}
		
			this.ctx = cvs.getContext('2d');
		}
		return this.ctx;
	},
	getSvg: function() {
		if (!this.svgObject) {
			var layer = this,
				div = layer.div,
				width = layer.width,
				height = layer.height;
			if (isIE) {
				// create xmlns if excanvas hasn't done it
		        if (!doc.namespaces["g_vml_"]) {
					doc.namespaces.add("g_vml_", "urn:schemas-microsoft-com:vml");
					// setup default css
					doc.createStyleSheet().cssText = "g_vml_\\:*{behavior:url(#default#VML)}";
		        }
				this.svgObject = createElement(DIV, null, {
					width: width + PX,
					height: height + PX,
					position: ABSOLUTE
				}, div);
	
		    
			} else {
				// create an object and inject SVG into it
				this.svgObject = createElement('object', { 
					width: width,
					height: height,
					type: 'image/svg+xml'
				}, {
					position : ABSOLUTE,
					left: 0,
					top: 0
				}, div);
			}
		}
		return this.svgObject;
	},
	drawLine: function(x1, y1, x2, y2, color, width) {
		var ctx = this.getCtx(), xBefore = x1;
		
		// normalize to a crisp line
		if (x1 == x2) x1 = x2 = mathRound(x1) + (width % 2 / 2);
		if (y1 == y2) y1 = y2 = mathRound(y1) + (width % 2 / 2);
		
		// draw path
		ctx.lineWidth = width;
		ctx.lineCap = 'round'; // If this is not set, plotBands appear
			// like 'square' in Firefox/Win. Reason unknown. 
		ctx.beginPath();
		ctx.moveTo(x1, y1);
		ctx.strokeStyle = color;
		ctx.lineTo(x2, y2);
		ctx.closePath();
		ctx.stroke();
	},
	drawPolyLine: function(points, color, width, shadow, fillColor) {
		var ctx = this.getCtx(),
			shadowLine = [];
		
		// the shadow
		if (shadow && width) {
			each (points, function(point) { // add 1px offset
				shadowLine.push(point === undefined ? point : point + 1);
			}); 
			for (var i = 1; i <= 3; i++) // three lines of differing thickness and opacity
				this.drawPolyLine(shadowLine, 'rgba(0, 0, 0, '+ (0.05 * i) +')', 6 - 2 * i);
		}
		
		// the line path
		ctx.beginPath();
		for (i = 0; i < points.length; i += 2) 
			ctx[i == 0 ? 'moveTo' : 'lineTo'](points[i], points[i + 1]);
		
		
		// common properties
		extend(ctx, {
			lineWidth: width,
			lineJoin: 'round'
		});
		
		// stroke 
	    if (color && width)	{
			ctx.strokeStyle = color; 
			ctx.stroke();
		}
		
		// fill
		if (fillColor) {
			ctx.fillStyle = fillColor;
			ctx.fill();
		}
		
	},
	drawRect: function(x, y, w, h, color, width, radius, fill, shadow, image) {
		// must (?) be done twice to apply both stroke and fill in excanvas
		function drawPath() {
			ctx.beginPath();
			if (!radius) {
				ctx.rect(x, y, w, h);
			} else {
				ctx.moveTo(x, y + radius);
				ctx.lineTo(x, y + h - radius);
				ctx.quadraticCurveTo(x, y + h, x + radius, y + h); // change: use bezier
				ctx.lineTo(x + w - radius, y + h);
				ctx.quadraticCurveTo(x + w, y + h, x + w, y + h - radius);
				ctx.lineTo(x + w, y + radius);
				ctx.quadraticCurveTo(x + w, y , x + w - radius, y);
				ctx.lineTo(x + radius, y);
				ctx.quadraticCurveTo(x , y, x, y + radius);
			}
			ctx.closePath();
		};
		
		var ctx = this.getCtx(), normalizer = (width || 0) % 2 / 2;

		// normalize for sharp edges
		x = mathRound(x) + normalizer;
		y = mathRound(y) + normalizer;
		w = mathRound(w);
		h = mathRound(h);
		
		// apply the drop shadow
		if (shadow) for (var i = 1; i <= 3; i++) {
	    	this.drawRect(x + 1, y + 1, w, h, 'rgba(0, 0, 0, '+ (0.05 * i) +')', 
	    		6 - 2 * i, radius);
		}

		// apply the background image behind everything
		if (image) ctx.drawImage(image, x, y, w, h);
		
		drawPath();
		
		if (fill) {

			ctx.fillStyle = setColor(fill, ctx);
			ctx.fill();
			// draw path again
			if (win.G_vmlCanvasManager) drawPath();
		}
		if (width) {
			ctx.strokeStyle = color;
			ctx.lineWidth = width;
			ctx.stroke();
		}
	},
	drawSymbol: function(symbol, x, y, radius, lineWidth, lineColor, fillColor) {
		var ctx = this.getCtx(),
			imageRegex = /^url\((.*?)\)$/;
		ctx.beginPath();
		
		if (symbol == 'square') {
			var len = .707 * radius;
			ctx.moveTo(x - len, y - len);
			ctx.lineTo(x + len, y - len);
			ctx.lineTo(x + len, y + len);
			ctx.lineTo(x - len, y + len);
			ctx.lineTo(x - len, y - len);
			
		} else if (symbol == 'triangle') {
			y++;
			ctx.moveTo(x, y - 1.33 * radius);
			ctx.lineTo(x + radius, y + .67 * radius);
			ctx.lineTo(x - radius, y + .67 * radius);
			ctx.lineTo(x, y - 1.33 * radius);
			
		} else if (symbol == 'triangle-down') {
			y--;
			ctx.moveTo(x, y + 1.33 * radius);
			ctx.lineTo(x - radius, y - .67 * radius);
			ctx.lineTo(x + radius, y - .67 * radius);
			ctx.lineTo(x, y + 1.33 * radius);
			
		} else if (symbol == 'diamond') {
			ctx.moveTo(x, y - radius);
			ctx.lineTo(x + radius, y);
			ctx.lineTo(x, y + radius);
			ctx.lineTo(x - radius, y);
			ctx.lineTo(x, y - radius);
			
		} else if (imageRegex.test(symbol)) {
			createElement('img', {
				onload: function() {
					var img = this,
						size = symbolSizes[img.src] || [img.width, img.height];
					setStyles(img, {
						left: mathRound(x - size[0] / 2) + PX,
						top: mathRound(y - size[1] / 2) + PX,
						visibility: VISIBLE
					})
					// Bug workaround: Opera (10.01) fails to get size the second time
					symbolSizes[img.src] = size;
				},
				src: symbol.match(imageRegex)[1]
			}, {
				position: ABSOLUTE,
				visibility: isIE ? VISIBLE : HIDDEN // hide until left and top are set in Gecko
			}, this.div);
			
		} else { // default: circle
			ctx.arc(x, y, radius, 0, 2*math.PI, true);
		} 
	
		if (fillColor) {
			ctx.fillStyle = fillColor;
			ctx.fill();
			
			// draw path again
			//if (isIE) ctx.arc(x, y, radius, 0, 2*Math.PI, true);
		}
		if (lineColor && lineWidth) {
			ctx.strokeStyle = lineColor || "rgb(100, 100, 255)";
	    	ctx.lineWidth = lineWidth || 2;
	    	ctx.stroke();
		}
	},
	drawHtml: function(html, attributes, styles) {
		createElement(
			DIV, 
			extend(attributes, { innerHTML: html }), 
			extend(styles, { position: ABSOLUTE}),
			this.div
		);
	},
	/**
	 * Add text and draw it. For those browsers adding the text to an SVG object,
	 * it is better for performance to add all strings before the object 
	 * is created. This function takes the same arguments as addText.
	 * 
	 * @param {string} str
	 * @param {number} x
	 * @param {number} y
	 * @param {object} style
	 * @param {number} rotation
	 * @param {string} align
	 */
	drawText: function() {
		this.addText.apply(this, arguments);
		this.strokeText();
	},
	addText: function(str, x, y, style, rotation, align) {
		if (str || str === 0) {
			
			// declare variables
			var layer = this,
				hasObject,
				div = layer.div,
				CSStransform,
				css = '', 
				style = style || {},
				fill = style.color || '#000000',
				align = align || 'left',
				fontSize = parseInt(style.fontSize || style.font.replace(/^[a-z ]+/, '')),
				span,
				spanWidth,
				transformOriginX;
		
			
			// prepare style
			for (var key in style) css += hyphenate(key) +':'+ style[key] +';';
			
			// what transform property is supported?
			each (['MozTransform', 'WebkitTransform', 'transform'], function(str) {
				if (str in div.style) CSStransform = str;
			});
			
			// if the text is not rotated, or if the browser supports CSS transform,
			// write a simple span
			if (!rotation || CSStransform) {
				span = createElement('span', {
					innerHTML: str
				}, extend(style, {
					position: ABSOLUTE,
					left: x + PX,
					whiteSpace: 'nowrap',
					bottom: mathRound(layer.height - y - fontSize * .25) + PX,
					color: fill
				}), div);
				
				// fix the position according to align and rotation
				spanWidth = span.offsetWidth;
				
				if (align == 'right') setStyles(span, {
					left: (x - spanWidth) + PX
				});
				else if (align == 'center') setStyles(span, { 
					left: mathRound(x - spanWidth / 2) + PX
				});
				
				if (rotation) {  // ... and CSStransform
					transformOriginX = { left: 0, center: 50, right: 100 }[align]
					span.style[CSStransform] = 'rotate('+ rotation +'deg)';
					span.style[CSStransform +'Origin'] = transformOriginX +'% 100%';					
				}
				
			} else if (isIE) {
				// to achieve rotated text, the ie text is drawn on a vector line that
				// is extrapolated to the left or right or both depending on the 
				// alignment of the text
				hasObject = true;
				var radians = (rotation || 0) * math.PI * 2 / 360, // deg to rad
					costheta = mathCos(radians),
					sintheta = mathSin(radians),
					length = layer.width, // the text is not likely to be longer than this
					baselineCorrection = fontSize / 3 || 3,
					left = align == 'left',
					right = align == 'right',
					x1 = left ? 	x : x - length * costheta,
					x2 = right ?	x : x + length * costheta,
					y1 = left ? 	y : y - length * sintheta,
					y2 = right ?	y : y + length * sintheta;
					
					
				// IE seems to always draw the text with v-text-align middle, so we need 
				// to correct for that by moving the path
				x1 += baselineCorrection * sintheta;
				x2 += baselineCorrection * sintheta;
				y1 -= baselineCorrection * costheta;
				y2 -= baselineCorrection * costheta;
				
				if (mathAbs(x1 - x2) < .1) x1 += .1; // strange IE painting bug
				if (mathAbs(y1 - y2) < .1) y1 += .1; // strange IE painting bug
				layer.svg += 
					'<g_vml_:line from="'+ x1 +', '+ y1 +'" to="'+ x2 +', '+ y2 +'" stroked="false">'+
						'<g_vml_:fill on="true" color="'+ fill +'"/>'+
						'<g_vml_:path textpathok="true"/>'+
						'<g_vml_:textpath on="true" string="'+ str +'" '+
							'style="v-text-align:'+ align + 
							';'+ css +'"/>'+
					'</g_vml_:line>';
			
			// svg browsers
			} else { 
				hasObject = true;
				layer.svg +=  
					'<g>'+
						'<text transform="translate('+ x +','+ y +
							') rotate('+ (rotation || 0) +')" '+
							'style="fill:'+ fill +';text-anchor:'+ 
							{ left: 'start', center: 'middle', right: 'end' }[align] +
				 			';'+ css.replace(/"/g, "'") +'">'+	str+
						'</text>'+
					'</g>';
			}
			
			layer.hasObject = hasObject;
		}
	},
	/*
	Experimental text rendering using canvas text. Speed and possibly weight are the advantages.
	Excanvas trunk supports canvas text, but not current version (2009-11-03). Older Gecko and Webkit
	browsers and Opera needs SVG approach, so all in all there is not much weight spared by this one.
	Furthermore, CanvasText looks crappy in Firefox, but on the other hand, SVG object make the tooltip
	animation slow.
	
	2009-11-07: Preliminary conclusion:
		- IE: Use VML on a textpath. The con is that IE8 renders all text as bold italic. Possibly
			experiment with CSS text rotation for whole 90 degrees if that's supported by IE8.
		- Firefox >= 3.5 (Gecko 1.9.1 was it?) and Webkit > ?: Use CSS transforms to rotate the text.
			Canvas and textFill would be a better and faster alternative, but the text is very badly
			drawn in Firefox. When that's fixed in future versions, go for Canvas and textFill instead.
		- Opera, older Gecko and older Webkit: Use SVG. It is slow, and all the text has to be added 
			before written to the SVG object. Hence the addText and strokeText functions. When Opera 
			starts supporting textFill or text rotate, use that instead.
		
	
	_addText: function(str, x, y, style, rotation, anchor) {
		if (str || str === 0) {
		
			// declare variables
			var css = '', 
				style = style || {},
				fill = style.color || '#000000',
				anchor = anchor || 'start',
				ctx,
				span,
				font = (style.font || '') +' '+ (style.fontSize || '') +' '+ 
					(style.fontWeight || '') +' '+ (style.fontFamily || ''),
				align = { start: 'left', middle: 'center', end: 'right' }[anchor],
				rotation = (rotation || 0) * math.PI * 2 / 360, // deg to rad
				fontSize = parseInt(style.fontSize || font); 
				
			// prepare style
			for (var key in style) css += hyphenate(key) +':'+ style[key] +';';
			
			if (rotation) {
				var ctx = this.getCtx();
				ctx.font = font;
				ctx.fillStyle = fill;
				ctx.textAlign = align;
				
				ctx.translate(x, y);
				ctx.rotate(rotation);
				ctx.fillText(str, 0, 0);
				ctx.rotate(-rotation);
				ctx.translate(-x, -y);
			} else {

			}
			
		}
	},*/
	strokeText: function() {
		if (this.hasObject) {
			var svgObject = this.getSvg(),
				svg = this.svg;
			if (isIE) {
				svgObject.innerHTML = svg;
			} else {
				svgObject.data = 
					'data:image/svg+xml,'+ svg +'</svg>';
					
				// append it again for Chrome to update
				if (isWebKit) this.div.appendChild(svgObject);
	
			}
		}
	},
	clear: function() {
		var layer = this,
			div = this.div,
			childNodes = div.childNodes,
			node;
		if (layer.ctx) layer.ctx.clearRect(0, 0, layer.width, layer.height);
		if (layer.svgObject) {
			div.removeChild(layer.svgObject);
			layer.svgObject = null;	
		}
		
		for (var i = childNodes.length - 1; i >= 0; i--) {
			node = childNodes[i];			
			if (node.tagName == 'SPAN') div.removeChild(node);
		} 
		
	},
	hide: function() {
		// Note: display:none doesn't work well in IE8. Hover states not showing
		// However, display:none is needed to hide the state levels. 
		//this.div.style.visibility = HIDDEN;
		setStyles(this.div, {
			visibility: HIDDEN,
			display: 'none'
		});
	},
	show: function() {
		//this.div.style.visibility = VISIBLE;
		setStyles(this.div, {
			visibility: VISIBLE,
			display: ''
		});
	}
};


function Chart (options) {
	/**
	 * Function: (private) addSeries
	 * 
	 * Initialize the series
	 */
	function addSeries() {
		var typeClasses = {
				line: LineSeries,
				spline: SplineSeries,
				area: AreaSeries,
				areaspline: AreaSplineSeries,
				column: ColumnSeries,
				bar: BarSeries,
				pie: PieSeries,
				scatter: ScatterSeries
			}, 
			typeClass,
			serie;
		each (options.series, function(serieOptions) {
			typeClass = typeClasses[serieOptions.type || optionsChart.defaultSeriesType];
			serie = new typeClass();
			serie.init(chart, serieOptions);
			series.push(serie);
		});
	};
	
	/**
	 * Function (private) getAxes
	 * 
	 * Return the extreme x and y values of all series prior to rendering the series
	 */
	function getAxes() {
		var needsAxes, // no axes untill a line/column etc. series comes by
			xMin,
			xMax,
			yMin,
			yMax,
			stack = [];
		each(series, function(serie) {
			var stacking = serie.options.stacking,
				usePercentage = stacking == 'percent';

			// create a stack for this particular series type
			if (stacking) 
				var typeStack = stack[serie.type] = (stack[serie.type] || []);

			if (usePercentage) {
				yMin = 0;
				yMax = 99;			
			} 
			if (serie.needsAxes) { // line, column etc. need axes, pie doesn't
				needsAxes = true;
				each(serie.data, function(point, i) {
					// initial values
					if (xMin === undefined) xMin = xMax = point[0];
					if (yMin === undefined) yMin = yMax = 
						/(column|bar)/.test(serie.type) ? 0 : point[1];

					// x axis
					if (point[0] > xMax) xMax = point[0];
					else if (point[0] < xMin) xMin = point[0];
					
					// y axis
					if (stacking) 
						typeStack[i] = typeStack[i] ? typeStack[i] + point[1] : point[1];
					
					var stackedPoint = typeStack ? typeStack[i] : point[1];
					if (!usePercentage) {
						if (stackedPoint > yMax) yMax = stackedPoint;
						else if (stackedPoint < yMin) yMin = stackedPoint;
					}
					
					if (stacking) stacks[serie.type][point[0]] = { 
						total: stackedPoint,
						cum: stackedPoint 
					};
				});
			}
			
		});
		return {
			show: needsAxes,
			xMin: xMin,
			xMax: xMax,
			yMin: yMin,
			yMax: yMax			
		}
	};
	/**
	 * Load graphics and data required to draw the chart
	 */
	function checkResources() {
		var allLoaded = true;
		for (var n in chart.resources) {
			if (!chart.resources[n]) allLoaded = false;
		}
		if (allLoaded) resourcesLoaded();
	}
	
	
	function zoom(selection) {
		// TODO: don't zoom if axis is categorized
		// update axes
		if (!selection) {
			xAxis.reset();
			yAxis.reset();
		} else {
			if (chart.tracker.zoomX) xAxis.setExtremes(selection.xMin, selection.xMax);
			if (chart.tracker.zoomY) yAxis.setExtremes(selection.yMin, selection.yMax);
		}
		
		// hide tooltip if present
		chart.tracker.tooltip.hide();
		
		// reset stacks
		for (var type in stacks) each (stacks[type], function(stack, i) {
			var total = stack.total;
			stacks[type][i] = { 
				total: total,
				cum: total
			}
		});
			
		
		// re-translate series to new axes extremes
		each(chart.series, function(serie) {
			each(serie.areas, function(area) {
				area.parentNode.removeChild(area);
			});
		
			serie.translate();
			serie.createArea();
			serie.clear();
			if (serie.type == 'spline') serie.getSplineData();
		});
		
		// clear chart area
		//backgroundLayer.clear();
		
		// remove labels
		/*var nodes = container.childNodes;
		for (var i = nodes.length - 1; i >= 0; i--) {
			if (/highchart-[xy]-axis-label/.test(nodes[i].className))
				container.removeChild(nodes[i]);
		};*/
		
		//render();
		// Axes
		if (chart.axes.show) {
			xAxis.render();
			yAxis.render();
		}
	
		// The series
		for (i = 0; i < series.length; i++) series[i].render();
		

	}
	
	/**
	 * Function: (private) showTitle
	 * 
	 * Show the title and subtitle of the chart
	 */
	function showTitle () {
		if (!chart.titleLayer) {
			var titleLayer = chart.titleLayer = new Layer('title-layer', container, null, {
				zIndex: 5
			});
			
			// title
			if (options.title) createElement('h2', {
				className: 'highcharts-title',
				innerHTML: options.title.text
			}, options.title.style, titleLayer.div);
			
			// subtitle
			if (options.subtitle) createElement('h3', {
				className: 'highcharts-subtitle',
				innerHTML: options.subtitle.text
			}, options.subtitle.style, titleLayer.div);
		}
	}
	/**
	 * Function: (private) resourcesLoaded
	 * 
	 * Prepare for first rendering after all data are loaded
	 */
	function resourcesLoaded() {
		
		chart.axes = getAxes();
	
		// Add the axises
		chart.xAxis = xAxis = new Axis(chart, options.xAxis, true);
		chart.yAxis = yAxis = new Axis(chart, options.yAxis);
		
		
		// Prepare for the axis sizes
		each(series, function(serie) {
			serie.translate();
			if (options.tooltip.enabled) serie.createArea();
		});	
		
		chart.render = render;
		chart.zoom = zoom;
		setTimeout(render, 0); // IE(7) needs timeout
	}
	/**
	 * Function: (private) render
	 * 
	 * Render all graphics for the chart
	 * 
	 */
	function render () {
		var mgn, div, i, labels = options.labels, credits = options.credits;
		
		setStyles(container, extend({
			position: RELATIVE,
			overflow: HIDDEN
		}, optionsChart.style));
		if (optionsChart.className) container.className += ' '+ optionsChart.className;
		
		// Chart area
		mgn = 2 * (optionsChart.borderWidth || 0) + (optionsChart.shadow ? 8 : 0);
		backgroundLayer.drawRect(mgn / 2, mgn / 2, chartWidth - mgn, chartHeight - mgn, 
			optionsChart.borderColor, optionsChart.borderWidth, optionsChart.borderRadius, 
			optionsChart.backgroundColor, optionsChart.shadow);
		
		
		// Plot area
		backgroundLayer.drawRect(marginLeft, marginTop, plotWidth, 
			plotHeight, optionsChart.plotBorderColor, 
			optionsChart.plotBorderWidth, null, optionsChart.plotBackgroundColor, 
			optionsChart.plotShadow, plotBackground );
		
		// Axes
		if (chart.axes.show) {
			xAxis.render();
			yAxis.render();
		}
	
		// Title
		showTitle();
		
		
		// Labels
		if (labels.items)	each (labels.items, function () {
			var attributes = extend({ className: 'highcharts-label' }, this.attributes);
			plotLayer.drawHtml(this.html, attributes, extend(labels.style, this.style));
		});

		// The series
		for (i = 0; i < series.length; i++) series[i].render();
		
		// Legend
		chart.legend = new Legend(chart);

		
		// Toolbar (don't redraw)
		if (!chart.toolbar) chart.toolbar = Toolbar(chart);
		
		// Credits
		if (credits.enabled && !chart.credits) 
			chart.credits = createElement('a', {
				href: credits.href,
				innerHTML: '<span>'+ credits.text +'</span>'
			}, extend(credits.style, {
				zIndex: 8
			}), container);

	};
	
	/**
	 * Create a new axis object
	 * @param {Object} chart
	 * @param {Object} options
	 * @param {Object} horiz
	 */
	function Axis (chart, options, horiz) {
	
		/**
		 * Translate from axis value to pixel position on the chart
		 */
		function translate(val, reverse, cvsCoord) {
			var sign = 1,
				cvsOffset = 0;
			if (cvsCoord) {
				sign *= -1; // canvas coordinates inverts the value
				cvsOffset = axisLength;
			}
			if (reversed) {
				sign *= -1; // inverse axis inverts it again
				cvsOffset -= sign * axisLength;
			}
			
			if (reverse) return (val - 0) / transA + min; // from chart pixel to value
			return sign * (val - min) * transA + cvsOffset; // from value to chart pixel
		};
		
		/**
		 * Add a single line across the plot
		 */
		function addPlotLine(pos, color, width) {
			
			if (width) {
				var x1, 
					y1, 
					x2, 
					y2
					
				x1 = x2 = translate(pos) + transB;
				y1 = y2 = chartHeight - translate(pos) - transB;
				if (horiz) { 
					y1 = marginTop;
					y2 = chartHeight - marginBottom;
				} else {
					x1 = marginLeft;
					x2 = chartWidth - marginRight;	
				}
				axisLayer.drawLine(x1, y1, x2, y2, color, width);
				
			}
		};
		/**
		 * Add a masked band across the plot
		 * @param {Number} from chart axis value
		 * @param {Number} to chart axis value
		 * @param {String} color
		 */
		function addPlotBand(from, to, color) {
			/*var x = horiz ? translate(from) + transB : marginLeft, 
				y = horiz ? marginTop : translate(to) + transB,
				width = horiz ? (to - from) * transA : plotWidth,
				height = horiz ? plotHeight : (to - from) * transA;
			
			backgroundLayer.drawRect(x, y, width, height, null, null, null, color);*/  
			
			var width = (to - from) * transA;
			addPlotLine(from + (to - from) / 2, color, width);
			
		}
		
		/**
		 * Add a tick mark an a label
		 */
		function addTick(pos, tickPos, color, width, len, withLabel) {
			var x1, y1, x2, y2, str, labelOptions = o.labels;
			if (tickPos == 'inside') len = -len;
			x1 = x2 = translate(pos + tickmarkOffset) + transB;
			y1 = y2 = chartHeight - translate(pos + tickmarkOffset) - transB;
			if (horiz) {
				y1 = chartHeight - marginBottom;
				y2 = y1 + len;
			} else {
				x1 = marginLeft;
				x2 = x1 - len;	
			}
			if (width) axisLayer.drawLine(x1, y1, x2, y2, color, width);
			
			
			// write the label
			if (withLabel && labelOptions.enabled) {
				str = labelFormatter.call({ value: 
					(categories && categories[pos] ? categories[pos] : pos) 
				});
				if (str || str === 0) axisLayer.addText(
					str,
					x1 + labelOptions.x - (tickmarkOffset && horiz ? tickmarkOffset * transA * (reversed ? -1 : 1) : 0),
					y1 + labelOptions.y - (tickmarkOffset && !horiz ? tickmarkOffset * transA * (reversed ? 1 : -1) : 0),
					labelOptions.style, 
					labelOptions.rotation,
					labelOptions.align
				);
			}
			
		};
		
		/**
		 * Take an interval and normalize it to multiples of 1, 2, 2.5 and 5
		 * @param {Number} interval
		 */
		function normalizeTickInterval(interval, multiples) {
			var normalized;
			
			// round to a tenfold of 1, 2, 2.5 or 5
			magnitude = multiples ? 1 : math.pow(10, mathFloor(math.log(interval) / math.LN10));
			normalized = interval / magnitude;
			
			// multiples for a linear scale
			if (!multiples) multiples = [1, 2, 2.5, 5, 10];
			
			// normalize the interval to the nearest multiple
			for (var i = 0; i < multiples.length; i++) {
				interval = multiples[i];
				if (normalized <= (multiples[i] + (multiples[i+1] || multiples[i])) / 2) {
					break;
				}
			}
			
			// multiply back to the correct magnitude
			interval *= magnitude;
			return interval;
		};
	
		function setDateTimeTickPositions() {
			tickPositions = [];
			var oneSecond = 1000 / timeFactor,
				oneMinute = 60000 / timeFactor,
				oneHour = 3600000 / timeFactor,
				oneDay = 24 * 3600000 / timeFactor,
				oneWeek = 7 * 24 * 3600000 / timeFactor,
				oneMonth = 30 * 24 * 3600000 / timeFactor,
				oneYear = 31556952000 / timeFactor;
			
			var units = [[
				'second',						// unit name
				oneSecond,						// fixed incremental unit
				[1, 2, 5, 10, 15, 30]			// allowed multiples
			], [
				'minute',						// unit name
				oneMinute,				// fixed incremental unit
				[1, 2, 5, 10, 15, 30]			// allowed multiples
			], [
				'hour',							// unit name
				oneHour,			// fixed incremental unit
				[1, 2, 3, 4, 6, 8, 12]			// allowed multiples
			], [
				'day',							// unit name
				oneDay,		// fixed incremental unit
				[1, 2]							// allowed multiples
			], [
				'week',							// unit name
				oneWeek,	// fixed incremental unit
				[1, 2]							// allowed multiples
			], [
				'month',
				oneMonth,
				[1, 2, 3, 4, 6]
			], [
				'year',
				oneYear,
				null
			]];
			
			var unit = units[6]; // default unit is years
			var interval = unit[1], multiples = unit[2];
			
			// loop through the units to find the one that best fits the tickInterval
			for (var i = 0; i < units.length; i++)  {
				unit = units[i];
				interval = unit[1], multiples = unit[2];
				
				
				if (units[i+1]) {
					// lessThan is in the middle between the highest multiple and the next unit.
					var lessThan = (interval * multiples[multiples.length - 1] + 
								units[i + 1][1]) / 2;
							
					// break and keep the current unit
					if (tickInterval <= lessThan) break;
				}
			}
			
			// prevent 2.5 years intervals, though 25, 250 etc. are allowed
			if (interval == oneYear && tickInterval < 5 * interval)
				multiples = [1, 2, 5];
	
			// get the minimum value by flooring the date
			var multitude = normalizeTickInterval(tickInterval / interval, multiples);
			var minYear; // used in months and years as a basis for Date.UTC()
			var minDate = new Date(min * timeFactor);
			minDate.setUTCMilliseconds(0);
			
			if (interval >= oneSecond) // second
				minDate.setUTCSeconds(interval >= oneMinute ? 0 :
					multitude * mathFloor(minDate.getUTCSeconds() / multitude));
	
			if (interval >= oneMinute) // minute
				minDate.setUTCMinutes(interval >= oneHour ? 0 :
					multitude * mathFloor(minDate.getUTCMinutes() / multitude));
	
			if (interval >= oneHour) // hour
				minDate.setUTCHours(interval >= oneDay ? 0 :
					multitude * mathFloor(minDate.getUTCHours() / multitude));
	
			if (interval >= oneDay) // day
				minDate.setUTCDate(interval >= oneMonth ? 1 :
					multitude * mathFloor(minDate.getUTCDate() / multitude));
					
			if (interval >= oneMonth) { // month
				minDate.setUTCMonth(interval >= oneYear ? 0 :
					multitude * mathFloor(minDate.getUTCMonth() / multitude));
				minYear = minDate.getUTCFullYear();
			}
			
			if (interval >= oneYear) { // year
				minYear -= minYear % multitude;
				minDate.setUTCFullYear(minYear);
			}
			
			// week is a special case that runs outside the hierarchy
			if (interval == oneWeek) {
				// get start of current week, independent of multitude
				minDate.setUTCDate(minDate.getUTCDate() - minDate.getUTCDay() + 
					options.startOfWeek);
			}	
			
			
			// get tick positions
			var i = 1, // for sikkerheits skuld
				time = min = minDate.getTime() / timeFactor,
				minYear = minDate.getUTCFullYear(),
				minMonth = minDate.getUTCMonth();
			
			while (time < max && i < 100) {
				tickPositions.push(time);
				
				// if the interval is years, use Date.UTC to increase years
				if (interval == oneYear) {
					time = Date.UTC(minYear + i * multitude, 0) / timeFactor;
				
				// if the interval is months, use Date.UTC to increase months
				} else if (interval == oneMonth) {
					time = Date.UTC(minYear, minMonth + i * multitude) / timeFactor;
				
				// else, the interval is fixed and we use simple addition
				} else {
					time += interval * multitude;
				}
				
				i++;
			}
			max = time;
			
			// dynamic label formatter 
			if (!options.labels.formatter) labelFormatter = function() {
				return dateFormat(options.dateTimeLabelFormats[unit[0]], this.value, 1);
			}
			
		}
			
			
		// linear axis type
		function setLinearTickPositions() {
			tickPositions = [];
			// snap numerical axis to tick
			if (!categories) {
				min -= (min >= 0 ? min % tickInterval : tickInterval + min % tickInterval);
				if (max % tickInterval) max += tickInterval - max % tickInterval;
			}

			// populate the intermediate values
			var invMag = (magnitude < 1 ? 1 / magnitude : 1) * 10; // round off JS float errors
			for (var i = min; i <= max; i += tickInterval) 
				tickPositions.push(mathRound(i * invMag) / invMag);

			// pad categorised axis by half a unit
			if (categories) {
				 min -= .5;
				 max += .5;
			}

			// dynamic label formatter 
			if (!labelFormatter) labelFormatter = function() {
				return this.value;
			}
		};
		
		function setScale() {
			// initial min and max from the extreme data values
			if (min === null) min = (options.min === null) ? 
				chart.axes[xOrY + 'Min'] : options.min;
			if (max === null) max = (options.max === null) ? 
				chart.axes[xOrY + 'Max'] : options.max;
				
			// tickInterval
			if (categories) tickInterval = 1;
			else tickInterval = options.tickInterval == 'auto' ? 
					(max - min) * options.tickPixelInterval / axisLength : 
					options.tickInterval;
					
			if (options.type != 'datetime') // linear
				tickInterval = normalizeTickInterval(tickInterval);
						
			// minorTickInterval
			minorTickInterval = (options.minorTickInterval == 'auto' && tickInterval) ?
					tickInterval / 5 : options.minorTickInterval;
					
			// get fixed positions based on tickInterval
			if (options.type == 'datetime')	setDateTimeTickPositions();
			else setLinearTickPositions();
			
			// the translation factor used in translate function
			transA = axisLength / (max - min);
			
		};
		
		function setExtremes(newMin, newMax) {
			var offset;
			if (newMax - newMin > options.maxZoom) {
				min = newMin;
				max = newMax;
			} else { // maxZoom exceeded, just center the selection
				offset = (options.maxZoom - newMax + newMin) / 2;
				min = newMin - offset;
				max = newMax + offset;
			}
			setScale();
		};
		
		function reset() {
			min = max = tickInterval = minorTickInterval = tickPositions = null;
			setScale();
		}
		
		function render() {
			axisLayer.clear();
			
			// alternate grid color
			if (o.alternateGridColor) {
				each(tickPositions, function(pos, i) {
					if (i % 2 == 0 && pos < max) {
						addPlotBand(pos, tickPositions[i + 1] !== undefined ? tickPositions[i + 1] : max, 
							o.alternateGridColor);
					}
				});
			}
			
			// custom plot bands
			if (o.plotBands) each (o.plotBands, function() {
				addPlotBand(this.from, this.to, this.color);
			});
			
			// minor grid lines
			if (minorTickInterval && !categories) for (var i = min; i <= max; i += minorTickInterval) {
				addPlotLine(i, o.minorGridLineColor, o.minorGridLineWidth);
				if (o.minorTickWidth) addTick(i, o.minorTickPosition, o.minorTickColor, 
					o.minorTickWidth, o.minorTickLength);
			}
			// grid lines
			each(tickPositions, function(pos) {
				var tickmarkPos = pos + tickmarkOffset;
				addPlotLine(tickmarkPos, o.gridLineColor, o.gridLineWidth);
				addTick(pos, o.tickPosition, o.tickColor, o.tickWidth, o.tickLength, 
					!((pos == min && !o.showFirstLabel) || (pos == max && !o.showLastLabel)));
			});
			// axis line
			if (o.lineWidth) axisLayer.drawLine(
				marginLeft, 
				horiz ? chartHeight - marginBottom : marginTop, 
				horiz ? chartWidth - marginRight : marginLeft, 
				chartHeight - marginBottom, 
				o.lineColor, 
				o.lineWidth
			);
			// title
			if (o.title && o.title.enabled && o.title.text) {
				
				// compute anchor points for each of the title align options
				var margin = horiz ? marginLeft : marginTop,
					length = horiz ? plotWidth : plotHeight;
					
				// the position in the length direction of the axis
				var alongAxis = { 
					low: margin + (horiz ? 0 : length), 
					middle: margin + length / 2, 
					high: margin + (horiz ? length : 0)
				}[o.title.align];
				
				// the position in the perpendicular direction of the axis
				var offAxis = (horiz ? marginTop + plotHeight : marginLeft) +
					(horiz ? 1 : -1) * // horizontal axis reverses the margin
					//(o.title.side == 'inside' ? 1 : -1) * // inside reverses the margin
					o.title.margin 
					- (isIE ? parseInt(
						o.title.style.fontSize || o.title.style.font.replace(/^[a-z ]+/, '')
					) / 3 : 0); // preliminary fix for vml's centerline
				
				axisLayer.addText(
					o.title.text,
					horiz ? alongAxis : offAxis, // x
					horiz ? offAxis : alongAxis, // y
					o.title.style, 
					o.title.rotation || 0,
					{ low: 'left', middle: 'center', high: 'right' }[o.title.align]
				);
				
			}
			// stroke tick labels and title
			axisLayer.strokeText();
		};
		
		// privates
		if (inverted) horiz = !horiz;
		var o = {};
		for (var n in options) { o[n] = options[n] };
		var m = chart.m; 						// chart margins
		var xOrY = horiz ? (inverted ? 'y' : 'x') : (inverted ? 'x' : 'y');
		var isXAxis = (xOrY == 'x');
		var axisLength = horiz ? plotWidth : plotHeight;
		var transA;							 	// translation factor
		var transB = horiz ? marginLeft : marginBottom; 		// translation addend
		var axisLayer = new Layer('axis-layer', container, null, { zIndex: 4});
		var max = null;
		var min = null;
		var tickInterval;
		var minorTickInterval;
		var magnitude;
		var tickPositions; // array containing predefined positions
		var zoom = 1;
		//var axisLabelsLayer = new Layer((horiz ? 'x' : 'y') +'-axis-labels');
		var labelFormatter = options.labels.formatter; // can be overwritten by dynamic format
		// column plots are always categorized
		var categories = options.categories || (isXAxis && chart.columnCount); 
		var reversed = options.reversed;
		var tickmarkOffset = (categories && options.tickmarkPlacement == 'between') ? .5 : 0;
		//var hasWrittenTitle;
		
		// public
		this.render = render;
		this.addPlotLine = addPlotLine;
		this.translate = translate;
		this.setExtremes = setExtremes;
		this.reset = reset;
		this.categories = categories;
		
		// set min and max
		setScale();
	
	}; // end Axis
	
	function Toolbar(chart) {
		var toolbarLayer, buttons = {};
		
		toolbarLayer = new Layer('toolbar', container, null, { 
			zIndex: 1004, 
			width: 'auto', 
			height: 'auto'
		});
		
		
		function add(id, text, title, fn) {
			if (!buttons[id]) {
				var button = createElement(DIV, {
						innerHTML: text,
						title: title,
						onclick: fn
					}, extend(options.toolbar.itemStyle, { 	
						zIndex: 1003
					}), toolbarLayer.div);
				buttons[id] = button;
			}
		}
		function remove(id) {
			buttons[id].parentNode.removeChild(buttons[id]);
			buttons[id] = null;
		}
		
		// public
		return {
			add: add,
			remove: remove
		}
	};
	
	function MouseTracker (chart, options) {
		if (!options.enabled) return;
		
		// private
		var imagemap,		
			tooltip = Tooltip(options),
			img, activePoint,
			mouseIsDown, selectionStartX, selectionStartY, selectionMarker,
			zoomX = /x/.test(chart.options.chart.zoomType),
			zoomY = /y/.test(chart.options.chart.zoomType);
		// public
		this.imagemap = imagemap = createImageMap();
		this.tooltip = tooltip;
		this.zoomX = zoomX;
		this.zoomY = zoomY;
		
		setDOMEvents();
		
		// set the fixed interval ticking
		//setInterval(mouseTick, 32);
		setInterval(function() {
			if (tooltipTick) tooltipTick();
		}, 13);
		
		/**
		 * Set the JS events on the container element
		 */
		function setDOMEvents () {
			imagemap.onmousemove = img.onmousemove = function(e) {
				e = e ? e : win.event;
				e.returnValue = false;
				if (mouseIsDown) {
					
						
					if (zoomX) {
						var xSize = e.clientX - selectionStartX - position.x - marginLeft;
						setStyles(selectionMarker, {
							width: mathAbs(xSize) + PX,
							left: (xSize > 0 ? selectionStartX : selectionStartX + xSize) + PX
						});
					}
						
					if (zoomY) {
						var ySize = e.clientY - selectionStartY - position.y - marginTop;
						setStyles(selectionMarker, {
							height: mathAbs(ySize) + PX,
							top: (ySize > 0 ? selectionStartY : selectionStartY + ySize) + PX
						});
					}
				} else {
					onmousemove(e);
				}
			}
			imagemap.onmouseout = img.onmouseout = function(e) {
				e = e ? e : win.event;
				var	related = e.relatedTarget || e.toElement;
				if (related && related != img && related.tagName != 'AREA') {
					tooltip.hide();
					if (chart.hoverSeries) {
						chart.hoverSeries.setState();
						chart.hoverSeries = null;
						activePoint = null;
					}
				}
			}
			imagemap.onmousedown = img.onmousedown = function(e) {
				e = e ? e : win.event;
				if (zoomX || zoomY) {
					if (e.preventDefault) e.preventDefault();
					mouseIsDown = true;
					selectionStartX = e.clientX - position.x - marginLeft;
					selectionStartY = e.clientY - position.y - marginTop;
					if (!selectionMarker) selectionMarker = createElement(DIV, null, {
						position: ABSOLUTE,
						border: 'none',
						background: '#4572A7',
						opacity: .25,
						width: zoomX ? 0 : plotWidth + PX,
						height: zoomY ? 0 : plotHeight + PX
					});
					plotLayer.div.appendChild(selectionMarker);
				}
			}
			imagemap.onmouseup = img.onmouseup = function() {
				var selection,
					transX = chart.xAxis.translate, 
					transY = chart.yAxis.translate;
				mouseIsDown = false;
					
				if (selectionMarker && selectionMarker.offsetWidth > 10 && 
						selectionMarker.offsetHeight > 10) {
					selection = {
						xMin: transX(selectionMarker.offsetLeft, true),
						xMax: transX(selectionMarker.offsetLeft + 
							selectionMarker.offsetWidth, true),
						yMin: transY(plotHeight - selectionMarker.offsetTop - 
							selectionMarker.offsetHeight, true),
						yMax: transY(plotHeight - selectionMarker.offsetTop, true)
					};
					selectionMarker.parentNode.removeChild(selectionMarker);
					selectionMarker = null;
					
					// add button to zoom back
					chart.toolbar.add('zoom', 'Reset zoom', 'Reset zoom level 1:1', function() {
						chart.zoom(false);
						chart.toolbar.remove('zoom');
					});
					
					chart.zoom(selection);	
					
				}
			}
		};
		/**
		 * Set tracker variables on mouse move
		 */
		function onmousemove (e) {
			var point = chart.hoverPoint,
				series = chart.hoverSeries;
			
			if (series) {
		
				// if the point is an other than the active one, refresh
				if (!point) point = series.tooltipPoints[
					inverted ? 
					e.clientY - position.y - marginTop : 
					e.clientX - position.x - marginLeft
				];
			
				if (point != activePoint) {
					tooltip.refresh(point, series);
					activePoint = point;
				} 
				
			}
		};
		
		/* *
		 * Ticks with a fixed interval and looks for changes in mouse position
		 */
		/*function mouseTick () {
				
			

		};*/
		/*function _mouseTick () {
			var point = chart.hoverPoint,
				series = chart.hoverSeries,
				data,
				i,
				
				mousePos = inverted ? mouseY : mouseX,
				refreshed;
				
			
			if (series) {
				data = inverted ? reverseArray(series.data) : series.data;
				if (!point) {
					// traverse to find closest non-null point
					for (i = 0; i < data.length; i++) {
						if (data[i].clientX > mousePos && data[i][1] !== null) break;
					}
					// nearest point above
					point = data[i] || data[i-1];
				}
		
				if (point) {
					// is the nearest non-null point below closer?
					var below = i - 1;
					while (data[below] && data[below][1] === null) below -= 1;
					if (data[below]) {
						if (mathAbs(mousePos - data[below].clientX)
								< mathAbs(mousePos - point.clientX))
							point = data[below];
					}
		
					// if the point is an other than the active one, refresh
					if (point != activePoint) {
						tooltip.refresh(point, series);
						activePoint = point;
						refreshed = true;
					} 
					
					//mouseMoved = false;
				}
			}
		};*/
		/**
		 * Create the image map that listens for mouseovers
		 */
		function createImageMap () {
			var id = 'highchartsMap'+ canvasCounter++;
			img = createElement('img', {
				useMap: '#'+ id
			}, {
				width: plotWidth + PX,
				height: plotHeight + PX,
				left: marginLeft + PX,
				top: marginTop + PX,
				opacity: 0,
				border: 'none',
				position: ABSOLUTE,
				// Workaround: if not clipped, the left axis will flicker in 
				// IE8 when hovering the chart
				clip: 'rect(1px,'+ plotWidth +'px,'+ plotHeight +'px,1px)', 
				zIndex: 9
			}, container);
			
			// Blank image for modern browsers. IE doesn't need a valid 
			// image for the image map to work, and fails in SSL mode
			// if it's present.
			if (!isIE) img.src = 'data:image/gif;base64,R0lGODlhAQABAJH/AP///0////'
			
			return createElement('map', {
				name: id,
				id: id
			}, null, container);
		};
	};
	

	
	/**
	 * The overview of the chart's series
	 * @param {Object} chart
	 */
	var Legend = function(chart) {
		// already existing			
		if (chart.legend) return;
			
		var li,
			options = chart.options.legend,
			layout = options.layout,
			symbolWidth = options.symbolWidth,
			dom,
			allItems = [],
			legendLayer = new Layer('legend', container, null, { zIndex: 5 });
			
		if (!options.enabled) return;
		
		// Don't use Layer prototype because this needs to sit above the chart in zIndex
		dom = createElement(DIV, {
			className: 'highcharts-legend highcharts-legend-'+ layout,
			innerHTML: '<ul style="margin:0;padding:0"></ul>'
		}, extend({
			position: ABSOLUTE,
			zIndex: 10
		}, options.style), container);
			
			
			
		
		// add HTML for each series
		each(chart.series, function(serie) {
			if (!serie.options.showInLegend) return;
			
			// use points or series for the legend item depending on legendType
			var items = (serie.options.legendType == 'point') ?
					serie.data : [serie];
					
				
			each(items, function(item) {
				// let these series types use a simple symbol
				item.simpleSymbol = /(bar|pie|area|column)/.test(serie.type);
				
				var style = extend(options.itemStyle, {
					paddingLeft: (symbolWidth +	options.symbolPadding) + PX,
					cssFloat: layout == 'horizontal' ? 'left' : 'none'
				}); 
				
				
				// generate the list item
				item.legendItem = li = createElement('li', {
						innerHTML: options.labelFormatter.call(item),
						className: item.visible ? '' : HIDDEN
					}, 
					item.visible ? style : merge(style, options.itemHiddenStyle), 
					dom.firstChild
				);
				
				
				addEvent(li, 'mouseover', function() {
					if (item.visible) setStyles(this, options.itemHoverStyle);
					item.setState('hover');
				});
				addEvent(li, 'mouseout', function() {
					if (item.visible) setStyles(this, style);
					item.setState();
				});
				addEvent(li, 'click', function() {
					var visible = this.className;
					setStyles(this, visible ? style : options.itemHiddenStyle);
					item.setVisible(visible);
				});
				
				// add it all to an array to use below
				allItems.push(item);
			});
		});
		
		// draw the box around the legend
		if (options.borderWidth || options.backgroundColor) 
				legendLayer.drawRect(
			dom.offsetLeft, 
			dom.offsetTop,
			dom.offsetWidth, 
			dom.offsetHeight, 
			options.borderColor, 
			options.borderWidth, 
			options.borderRadius, 
			options.backgroundColor, 
			options.shadow
		);



		
		// Add the symbol after the list is complete. Use the background
		// layer because a separate legend layer will cover the chart. To 
		// allow changing of the graphics in future versions, a separate
		// legend-symbol-layer must be added just above the background and 
		// cleared between each redraw. Another idea would be to extend
		// the series layer over the entire chart area and draw the graphic
		// there. In that case the graph itself must be clipped to the 
		// plot area.
		each(allItems, function(item) {
			var li = item.legendItem,
				symbolX = dom.offsetLeft + li.offsetLeft,
				symbolY = dom.offsetTop + li.offsetTop + li.offsetHeight / 2,
				markerOptions;
				
				
			// draw the line
			if (!item.simpleSymbol && item.options && item.options.lineWidth)
				legendLayer.drawLine(
					symbolX, 
					symbolY, 
					symbolX + symbolWidth, 
					symbolY, 
					item.color, 
					item.options.lineWidth
				);
			// draw a simple symbol
			if (item.simpleSymbol) // bar|pie|area|column
				legendLayer.drawRect(
					symbolX,
					symbolY - 6,
					16,
					12,
					null,
					0,
					2,
					item.color
				);
			// draw the marker
			else if (item.options && item.options.marker && item.options.marker.enabled)
				item.drawMarker(
					legendLayer, 
					symbolX + symbolWidth / 2, 
					symbolY, 
					item.options.marker
				);
		});
	};
	
	function _Tooltip (options) {
		var currentSeries,
			innerDiv,
			borderWidth = options.borderWidth,
			boxLayer;
		
		/*tooltipDiv = createElement("div", null, {
			position: ABSOLUTE,
			visibility: HIDDEN,
			overflow: HIDDEN,
			padding: '0 50px 5px 0',
			zIndex: 6
		}, container),
		
		// the rounded corner box
		boxLayer = new Layer('tooltip-box', tooltipDiv, null, {
			width: plotWidth + PX,
			height: plotHeight + PX
		});
		
		// an inner element for the contents
		innerDiv = createElement("div", { 
				className: "highcharts-tooltip"
			}, extend(options.style, { 
				position: RELATIVE,
				zIndex: 2
			}), tooltipDiv
		);*/
		
		var stolpe = createElement(DIV, null, {
			position: ABSOLUTE,
			height: plotHeight + PX,
			borderLeft: '1px solid black'
		}, plotLayer.div);
		
		function refresh(point, series) {
			var tooltipPos = point.tooltipPos,
				chartOptions = chart.options,
				borderColor = options.borderColor || point.color || series.color || '#606060',
				categories = chartOptions.xAxis.categories,
				inverted = chartOptions.chart.inverted,
				x,
				y,
				boxX,
				boxY,
				boxWidth,
				boxHeight,
				text;
				
			stolpe.style.left = point.x + PX;
				
			
			// register the current series
			currentSeries = series;
			
			// update the inner HTML
			/*text = point.tooltipText;
			if (text === false) hide(); 
			else {
				innerDiv.innerHTML = text;
				
				// get the reference point coordinates (pie charts use tooltipPos)
				x = tooltipPos ? tooltipPos[0] : (inverted ? plotWidth - point.y : point.x);
				y = tooltipPos ? tooltipPos[1] : (inverted ? plotHeight - point.x : point.y);
				
				// Draw a rounded border. Draw the border with 20px extra width to minimize
				// the need to redraw it later. Next time, only redraw if the width of the 
				// box is more than 20px wider or smaller than the old box.
				boxWidth = innerDiv.offsetWidth - borderWidth;
				boxHeight = innerDiv.offsetHeight - borderWidth;
				if (boxWidth > (boxLayer.w || 0) + 20 || boxWidth < (boxLayer.w || 0) - 20 || 
						boxHeight > boxLayer.h || boxLayer.c != borderColor) {
				    boxLayer.clear();		
				    boxLayer.drawRect(
						borderWidth / 2, 
						borderWidth / 2, 
				    	boxWidth + 20,
				    	boxHeight, 
				    	borderColor, 
						borderWidth, 
						options.borderRadius, 
				    	options.backgroundColor, 
						options.shadow
					);
					
			
					// register size
					extend(boxLayer, {
						w: boxWidth,
						h: boxHeight,
						c: borderColor
					});
				}
				
				// keep the box within the chart area
				boxX = x - boxLayer.w + marginLeft - 35;
				if ((inverted || boxX < 5) && x + boxLayer.w < chartWidth - 100) 
					boxX = x + marginLeft + 15; // right align
				
				boxY = y - boxLayer.h + 10 + marginTop;
				
				if (boxY < 5) boxY = 5; // above
				else if (boxY + boxLayer.h > chartHeight) boxY = chartHeight - boxLayer.h - 5; // below
				
				// do the move
				move(mathRound(boxX), mathRound(boxY));
		
		
				
			    // show the hover mark
				series.drawPointState(point, 'hover');
			
				tooltipDiv.style.visibility = "visible";
			}*/
		
		};
		
		// Provide a soft movement of the tooltip
		function move(finalX, finalY) {
			
			/*var hidden = (tooltipDiv.style.visibility == HIDDEN),
				x = hidden ? finalX : (tooltipDiv.offsetLeft + finalX) / 2, 
				y = hidden ? finalY : (tooltipDiv.offsetTop + finalY) / 2;
			
			setStyles( tooltipDiv, { left: x +"px", top: y +"px" });
			
			// run on next tick of the mouse tracker
			if (mathAbs(finalX - x) > 1 || mathAbs(finalY - y) > 1) {
				//tooltipTick = function() {
					move(finalX, finalY);
				//};
			} else {
				tooltipTick = null;
			}*/
		};
		function hide() {
			//if (this.chart.hoverMarker) this.chart.hoverMarker.render();
			if (tooltipDiv) tooltipDiv.style.visibility = HIDDEN;
			//if (boxLayer) boxLayer.clear();
			if (currentSeries) currentSeries.drawPointState();
		};
		
		// public members
		return {
			refresh: refresh,
			hide: hide
		}		
	};
	
	function Tooltip (options) {
		var currentSeries,
			innerDiv,
			borderWidth = options.borderWidth,
			boxLayer;
		
		tooltipDiv = createElement(DIV, null, {
			position: ABSOLUTE,
			visibility: HIDDEN,
			overflow: HIDDEN,
			padding: '0 50px 5px 0',
			zIndex: 6
		}, container),
		
		// the rounded corner box
		boxLayer = new Layer('tooltip-box', tooltipDiv, null, {
			width: plotWidth + PX,
			height: plotHeight + PX
		});
		
		// an inner element for the contents
		innerDiv = createElement(DIV, { 
				className: "highcharts-tooltip"
			}, extend(options.style, { 
				position: RELATIVE,
				zIndex: 2
			}), tooltipDiv
		);
		
		
		function refresh(point, series) {
			var tooltipPos = point.tooltipPos,
				chartOptions = chart.options,
				borderColor = options.borderColor || point.color || series.color || '#606060',
				categories = chartOptions.xAxis.categories,
				inverted = chartOptions.chart.inverted,
				x,
				y,
				boxX,
				boxY,
				boxWidth,
				boxHeight,
				text = point.tooltipText;
				
			
			// register the current series
			currentSeries = series;
			
			// update the inner HTML
			/*text = options.formatter.call({
				seriesName: series.options.name,
				pointName: point.name,
				x: categories && categories[point[0]] !== undefined ? 
					categories[point[0]] : point[0], 
				y: point[1],
				percentage: point.percentage
			});*/
			if (text === false) { 
				hide(); 
			} else {
				innerDiv.innerHTML = text;
				
				// get the reference point coordinates (pie charts use tooltipPos)
				x = tooltipPos ? tooltipPos[0] : (inverted ? plotWidth - point.y : point.x);
				y = tooltipPos ? tooltipPos[1] : (inverted ? plotHeight - point.x : point.y);
				
				// Draw a rounded border. Draw the border with 20px extra width to minimize
				// the need to redraw it later. Next time, only redraw if the width of the 
				// box is more than 20px wider or smaller than the old box.
				boxWidth = innerDiv.offsetWidth - borderWidth;
				boxHeight = innerDiv.offsetHeight - borderWidth;
				if (boxWidth > (boxLayer.w || 0) + 20 || boxWidth < (boxLayer.w || 0) - 20 || 
						boxHeight > boxLayer.h || boxLayer.c != borderColor) {
				    boxLayer.clear();		
				    boxLayer.drawRect(
						borderWidth / 2, 
						borderWidth / 2, 
				    	boxWidth + 20,
				    	boxHeight, 
				    	borderColor, 
						borderWidth, 
						options.borderRadius, 
				    	options.backgroundColor, 
						options.shadow
					);
					
					// register size
					extend(boxLayer, {
						w: boxWidth,
						h: boxHeight,
						c: borderColor
					});
				}
				
				// keep the box within the chart area
				boxX = x - boxLayer.w + marginLeft - 35;
				if ((inverted || boxX < 5) && x + boxLayer.w < chartWidth - 100) 
					boxX = x + marginLeft + 15; // right align
				
				boxY = y - boxLayer.h + 10 + marginTop;
				
				if (boxY < 5) boxY = 5; // above
				else if (boxY + boxLayer.h > chartHeight) 
					boxY = chartHeight - boxLayer.h - 5; // below
				
				// do the move
				move(mathRound(boxX), mathRound(boxY));
		
			    // show the hover mark
				series.drawPointState(point, 'hover');
			
				tooltipDiv.style.visibility = VISIBLE;
			}
		
		};
		
		// Provide a soft movement of the tooltip
		function move(finalX, finalY) {
			
			var hidden = (tooltipDiv.style.visibility == HIDDEN),
				x = hidden ? finalX : (tooltipDiv.offsetLeft + finalX) / 2, 
				y = hidden ? finalY : (tooltipDiv.offsetTop + finalY) / 2;
			
			setStyles( tooltipDiv, {
				left: x + PX, 
				top: y + PX
			});
			
			// run on next tick of the mouse tracker
			if (mathAbs(finalX - x) > 1 || mathAbs(finalY - y) > 1) {
				//tooltipTick = function() {
					move(finalX, finalY);
				//};
			} else {
				tooltipTick = null;
			}
		};
		function hide() {
			if (tooltipDiv) tooltipDiv.style.visibility = HIDDEN;
			if (currentSeries) currentSeries.drawPointState();
		};
		
		// public members
		return {
			refresh: refresh,
			hide: hide
		}		
	};
	
		
	

		
	//--- initialize Chart ---
	// Override defaults for inverted axis
	if (options.chart && options.chart.inverted) defaultOptions = 
		merge(defaultOptions, invertedDefaultOptions);
	
	// Handle regular options
	options = merge(defaultOptions, options);
	var optionsChart = options.chart;
		
	// handle margins
	var margin = optionsChart.margin;
	if (typeof margin == 'number') 
		margin = [ margin, margin, margin, margin ];
	
	
	var chart = this,
		container = doc.getElementById(optionsChart.renderTo),
		marginTop = margin[0],
		marginRight = margin[1],
		marginBottom = margin[2],
		marginLeft = margin[3],
		backgroundLayer = new Layer('chart-background', container),
		chartHeight, 
		chartWidth,
		plotLayer,
		plotHeight,
		plotWidth,
		//ctx, 
		tracker,
		xAxis, 
		yAxis, 
		series = [], 
		position = getPosition(container), 
		resourcesLoaded, 
		plotBackground,
		inverted,
		tooltipTick,
		tooltipDiv,
		stacks;
		
	// Set to zero for each new chart
	colorCounter = 0;
	symbolCounter = 0;
	
	// Update position on resize
	addEvent(win, 'resize', function() {
		position = getPosition(container);
	});
	
	// Chart member functions
	chart.addLoading = function (loadingId) {
		chart.resources[loadingId] = false;
	}
	chart.clearLoading = function (loadingId) {
		chart.resources[loadingId] = true;
		checkResources();
	}
	
	
	chart.options = options;
	chart.series = series;
	
	chart.resources = {};
	
	
	chart.inverted = inverted = options.chart.inverted
	
	chart.chartWidth = chartWidth = container.offsetWidth;
	chart.chartHeight = chartHeight = container.offsetHeight;
	
	chart.plotWidth = plotWidth = chartWidth - marginLeft - marginRight;
	chart.plotHeight = plotHeight = chartHeight - marginTop - marginBottom;
	
	chart.plotLeft = marginLeft;
	chart.plotTop = marginTop;
	
	
	// create the stack cumulative lookup arrays
	chart.stacks = stacks = {
		bar: [],
		column: [],
		area: [],
		areaspline: []
	};
	
	// create the plot area
	chart.plotLayer = plotLayer = new Layer('plot', container, null, {
		position: ABSOLUTE,
		width: plotWidth + PX,
		height: plotHeight + PX,
		left: marginLeft + PX,
		top: marginTop + PX,
		overflow: HIDDEN,
		zIndex: 6
	});
	this.tracker = new MouseTracker(chart, options.tooltip);
	
	// Wait for loading of plot area background
	if (optionsChart.plotBackgroundImage) {
		chart.addLoading('plotBack');
		plotBackground = createElement('img');
		plotBackground.onload = function() {
			chart.clearLoading('plotBack');
		}
		plotBackground.src = optionsChart.plotBackgroundImage;
	}
	
	// Initialize the series
	addSeries();
	checkResources();
};

/**
 * The base function which all other series types inherit from
 * @param {Object} chart
 * @param {Object} options
 */
var LineSeries = function() {
	this.needsAxes = true;
	this.type = 'line';		
};

LineSeries.prototype = {
	init: function(chart, options) {
		var series = this;
		series.chart = chart;
		options = series.setOptions(options); // merge with plotOptions
		
		extend (series, {
			options: options,
			name: options.name,
			state: '',
			visible: options.visible || options.visible === undefined
		});
		
		
		series.getColor();
		series.getSymbol();
		
		// get the data and go on when the data is loaded
		series.getData(options);
			
	},
	getData: function(options) {
		var series = this,
			chart = series.chart,
			loadingId = 'series'+ idCounter++;
		
		// Ajax loaded data
		if (!options.data && options.dataURL) {
			chart.addLoading(loadingId);
			getAjax(options.dataURL, function(data) {
				series.dataLoaded(data);
				chart.clearLoading(loadingId);
			});
		} else {
			series.dataLoaded(options.data);
		}
	},
	dataLoaded: function(data) {
		var series = this,
			chart = series.chart,
			options = series.options,
			//data = series.data,
			dataParser = options.dataParser,
			stateLayers = {},
			layerGroup,
			i;
		
		// if no dataParser is defined for ajax loaded data, assume JSON and eval the code
		if (options.dataURL && !dataParser) 
			dataParser = function(data){
				return eval(data);
			}
		// dataParser is defined, run parsing
		if (dataParser) data = dataParser.call(series, data);
		
		
		// create the group layer (TODO: move to render?)
		this.layerGroup = layerGroup = new Layer('series-group', chart.plotLayer.div, null, {
			zIndex: 2 // labels are underneath
		});
		each(['', 'hover'], function(state) { // create the state layers
			stateLayers[state] = new Layer('state-'+ state, layerGroup.div);
			if (state) stateLayers[state].hide(); // only normal state visible initially
		});
		this.stateLayers = stateLayers;
		
		if (!series.visible) series.setVisible(false);
		
		// normalize data
		i = options.pointStart || 0;
		data = map(data, function(point) {
			var ret = point, // twodimensional array [x, y]
				n;
			// onedimensional array input
			if (typeof point == 'number' || point === null) {
				ret = [i, point];	
			}
			
			// object input
			else if (typeof point == 'object' && typeof point.length != 'number') {
				// Point config object members:
				// x: x value
				// y: y value
				// name: Name for the point, used for tooltip or dataLabel
				// marker: Individual marker for the point
				// sliced: Pie chart only
				// TODO: dataLabels: Individual data label for the point
	
				// create an array with x and y as 0 and 1
				ret = [(point.x === undefined ? i : point.x), point.y];
				// copy additional properties
				for (n in point) ret[n] = point[n];	
			}
			
			// categorized data with name in first position
			else if (typeof point[0] == 'string') {
				point.name = point[0];
				point[0] = i;
				ret = point;
			}
			
			
			i += options.pointInterval || 1;
			return ret;
			
		});
		// reverse the data if the axis is reversed for the MouseTracker
		series.data = chart.options.xAxis.reversed ? reverseArray(data) : data;
		
		// get segments to handle null values
		var lastNull = -1,
			segments = [];
		each (data, function(point, i) {
			if (point[1] === null) {
				if (i > lastNull + 1)
					segments.push(data.slice(lastNull + 1, i));
				lastNull = i;	
			} else if (i == data.length - 1) { // last value
				segments.push(data.slice(lastNull + 1, i + 1));
			}
		});
		this.segments = segments;
		
		
	},
	setOptions: function(options){
		return merge(this.chart.options.plotOptions[this.type], options);
		
	},
	getColor: function(){
		var defaultColors = this.chart.options.colors;
		this.color = this.options.color || defaultColors[colorCounter++] || '#0000ff';
		if (colorCounter >= defaultColors.length) 
			colorCounter = 0;
	},
	getSymbol: function(){
		var defaultSymbols = this.chart.options.symbols,
			symbol = this.options.marker.symbol || 'auto';
		if (symbol == 'auto') symbol = defaultSymbols[symbolCounter++];
		this.symbol = symbol;
		if (symbolCounter >= defaultSymbols.length) 
			symbolCounter = 0;
	},
	/**
	 * Translate data points from raw values 0 and 1 to x and y
	 */
	translate: function(){
		var chart = this.chart, 
			series = this, 
			stacking = series.options.stacking, 
			stack = chart.stacks[series.type];
		each(this.data, function(pt, i){
			var xValue = pt[0], yValue = pt[1], yBottom, pointStack;
			pt.x = chart.xAxis.translate(pt[0]);
			
			// calculate the bottom y value for stacked series
			if (stacking) {
				pointStack = stack[xValue];
			
				yBottom = pointStack.cum = 
					pointStack.cum - yValue; // start from top
				yValue = yBottom + yValue;
				
				if (stacking == 'percent') {
					yBottom = yBottom * 100 / pointStack.total;
					yValue = yValue * 100 / pointStack.total;
					pt.percentage = pt[1] * 100 / pointStack.total;
				}
				pt.yBottom = chart.yAxis.translate(yBottom, 0, 1);
			}
			// set the y value
			if (yValue !== null) 
				pt.y = chart.yAxis.translate(yValue, 0, 1);
			
			// set client related positions for mouse tracking
			pt.clientX = chart.inverted ? 
				chart.plotHeight - pt.x + chart.plotTop : 
				pt.x + chart.plotLeft; // for mouse tracking
		});
		this.setTooltipPoints();
	},
	/**
	 * Memoize tooltip texts and positions
	 */
	setTooltipPoints: function() {
		var series = this,
			chart = series.chart,
			inverted = chart.inverted,
			categories = chart.xAxis.categories,
			data = [],
			plotSize = inverted ? chart.plotHeight : chart.plotWidth,
			low,
			high,
			tooltipPoints = []; // a lookup array for each pixel in the x dimension
			
		// concat segments to overcome null values
		each (series.segments, function(segment){
			data = data.concat(segment);
		});
		
		// loop the concatenated data and apply each point to all the closest
		// pixel positions
		if (chart.options.xAxis.reversed) data = reverseArray(data);
		each (data, function(point, i) {
			
			point.tooltipText = chart.options.tooltip.formatter.call({
				series: series,
				point: point,
				x: categories && categories[point[0]] !== undefined ? 
					categories[point[0]] : point[0], 
				y: point[1],
				percentage: point.percentage
			});
			
			low = data[i - 1] ? data [i - 1].high + 1 : 0;
			high = point.high = data[i + 1] ? (
				mathFloor((point.x + (data[i + 1] ? 
					data[i + 1].x : plotSize)) / 2)) :
					plotSize;
			
			while (low <= high) {
				tooltipPoints[inverted ? plotSize - low++ : low++] = point;
			}
		});
		series.tooltipPoints = tooltipPoints;
	},
	
	
	/**
	 * Draw the actual graph
	 */
	drawLine: function(state) {
		var i, 
			j, 
			series = this, 
			options = series.options, 
			chart = series.chart, 
			layer = series.stateLayers[state], 
			data = series.data, 
			color = options.lineColor || series.color, 
			fillColor = options.fillColor == 'auto' ? 
				Color(series.color).setOpacity(options.fillOpacity || .75).get() : 
				options.fillColor, 
			inverted = chart.inverted, 
			y0 = (inverted ? 0 : chart.chartHeight) - chart.yAxis.translate(0);
		
		// get state options
		if (state) 
			options = merge(options, options.states[state]);
		
		// initiate the animation
		if (series.animate) series.animate(true);
		
		// divide into segments
		each(series.segments, function(segment){
			var line = [], area = [];
			// get the points
			each(segment, function(point){
				line.push(
					inverted ? chart.plotWidth - point.y : point.x, 
					inverted ? chart.plotHeight - point.x : point.y
				);
			});
			
			// draw the area
			if (/area/.test(series.type)) {
				for (var i = 0; i < line.length; i++) 
					area.push(line[i]);
				if (options.stacking && series.type != 'areaspline') {
					// follow stack back. Todo: implement areaspline
					for (i = segment.length - 1; i >= 0; i--) 
						area.push(segment[i].x, segment[i].yBottom);
					
				
				} else { // follow zero line back
					area.push(
						inverted ? y0 : segment[segment.length - 1].x, 
						inverted ? segment[0].x : y0, 
						inverted ? y0 : segment[0].x, 
						inverted ? segment[segment.length - 1].x : y0
					);
				}
				layer.drawPolyLine(area, null, null, options.shadow, fillColor);
			}
			// draw the line
			if (options.lineWidth) layer.drawPolyLine(line, color, options.lineWidth, options.shadow);
		});
		
		// experimental animation
		if (series.animate) series.animate();
		
	},
	/**
	 * Experimental animation
	 */
	animate: function(init) {
		var series = this,
			div = series.layerGroup.div;
		
		if (init) { // initialize the animation
			setStyles (div, {
				overflow: HIDDEN,
				width: 0
			});
		} else { // run the animation
			animate(div, { width: series.chart.plotWidth + PX }, { duration: 1000 });
	
			// delete this function to allow it only once
			this.animate = null;
		}
		
	},
	
	/**
	 * Draw the markers
	 */
	drawPoints: function(state){
		var series = this, i,  //state = series.state,
		layer = series.stateLayers[state], 
			seriesOptions = series.options, 
			markerOptions = seriesOptions.marker, 
			data = series.data, 
			chart = series.chart, 
			inverted = chart.inverted;
		
		
		
		if (state) {
			// default hover values are dynamic based on basic state 
			var stateOptions = seriesOptions.states[state].marker;
			if (stateOptions.lineWidth === undefined) 
				stateOptions.lineWidth = markerOptions.lineWidth + 1;
			if (stateOptions.radius === undefined) 
				stateOptions.radius = markerOptions.radius + 1;
			markerOptions = merge(markerOptions, stateOptions);
		}
		
		if (markerOptions.enabled) {
			each(data, function(point){
				if (point.y !== undefined) 
					series.drawMarker(
						layer, 
						inverted ? chart.plotWidth - point.y : point.x, 
						inverted ? chart.plotHeight - point.x : point.y, 
						merge(markerOptions, point.marker)
					);
				
			});
		}
	},
	/**
	 * Some config objects, like marker, have a state value that depends on the base value
	 * @param {Object} props
	 */
	/*getDynamicStateValues: function(base, state, props) {
	 each (props, function(value, key) {
	 if (state[key] === undefined) state[key] = base[key] + value;
	 });
	 return state;
	 },*/
	/**
	 * Draw a single marker into a given layer and position
	 */
	drawMarker: function(layer, x, y, options){
		if (options.lineColor == 'auto') 
			options.lineColor = this.color;
		if (options.fillColor == 'auto') 
			options.fillColor = this.color;
		if (options.symbol == 'auto') 
			options.symbol = this.symbol;
		layer.drawSymbol(
			options.symbol, 
			x, 
			y, 
			options.radius, 
			options.lineWidth, 
			options.lineColor, 
			options.fillColor
		);
	},
	
	/**
	 * Draw the data labels
	 */
	drawDataLabels: function(){
		if (this.options.dataLabels.enabled && !this.hasDrawnDataLabels) {
			var series = this, 
				i, 
				x, 
				y, 
				data = series.data, 
				options = series.options.dataLabels, 
				color, 
				str, 
				dataLabelsLayer, 
				chart = series.chart, 
				inverted = chart.inverted,
				isPie = (series.type == 'pie');
			series.dataLabelsLayer = dataLabelsLayer = new Layer('data-labels', 
				series.layerGroup.div, 
				null, {
					zIndex: 1
				});
			options.style.color = options.color == 'auto' ? series.color : options.color;
			each(data, function(point){
				str = options.formatter.call({
					x: point[0],
					y: point[1],
					series: series,
					point: point
				});
				x = (inverted ? chart.plotWidth - point.y : point.x) + options.x;
				y = (inverted ? chart.plotHeight - point.x : point.y) + options.y;
				
				// special case for pies
				if (point.tooltipPos) {
					x = point.tooltipPos[0] + options.x;
					y = point.tooltipPos[1] + options.y;
				}
				// special for pies
				if (isPie) 
					dataLabelsLayer = new Layer('data-labels', point.layer.div, null, { zIndex: 3} );
				
				if (str) dataLabelsLayer[isPie ? 'drawText' : 'addText'](
					str, 
					x, 
					y, 
					options.style, 
					options.rotation, 
					options.align
				);
					
			});
			if (!isPie) dataLabelsLayer.strokeText();
			
			// only draw once - todo: different labels in different states and single point label?
			series.hasDrawnDataLabels = true;
		}
	},
	
	/**
	 * Draw a single point in hover state
	 */
	drawPointState: function(point, state){
		var chart = this.chart, 
			inverted = chart.inverted, 
			singlePointLayer = chart.singlePointLayer, 
			options = this.options,
			stateOptions;
		
		// a specific layer for the currently active point
		if (!singlePointLayer) 
			singlePointLayer = chart.singlePointLayer = new Layer(
				'single-point', 
				chart.plotLayer.div,
				null,
				{ zIndex: 3 }
			);
		singlePointLayer.clear();
		
		if (state) {
			// merge series hover marker and marker hover marker
			var seriesStateOptions = options.states[state].marker, 
				pointStateOptions = options.marker.states[state];
			if (pointStateOptions.radius === undefined) 
				pointStateOptions.radius = seriesStateOptions.radius + 2;
			stateOptions = merge(
				options.marker, 
				point.marker, 
				seriesStateOptions, 
				pointStateOptions
			);
			if (stateOptions && stateOptions.enabled) 
				this.drawMarker(
					singlePointLayer, 
					inverted ? chart.plotWidth - point.y : point.x, 
					inverted ? chart.plotHeight - point.x : point.y, 
					stateOptions
				);
		}
	},
	
	/**
	 * Render the graph and markers
	 */
	render: function(){
		
		this.drawDataLabels();
		
		for (var state in this.stateLayers) {
			this.drawLine(state);
			this.drawPoints(state);
		}
		
	},
	
	clear: function(){
		var stateLayers = this.stateLayers;
		for (var state in stateLayers) {
			stateLayers[state].clear();
			stateLayers[state].cleared = true;
		}
		if (this.dataLabelsLayer) {
			this.dataLabelsLayer.clear();
			this.hasDrawnDataLabels = false;
		}
	},
	
	/**
	 * Set the state of the graph and redraw
	 */
	setState: function(state){
		state = state || '';
		if (this.state != state) {
			
			var series = this, 
				stateLayers = series.stateLayers, 
				newStateLayer = stateLayers[state],
				oldStateLayer = stateLayers[series.state],
				singlePointLayer = series.singlePointLayer || series.chart.singlePointLayer;
			series.state = state;
			
			if (state) 
				newStateLayer.show();
			else {
				oldStateLayer.hide();
				if (singlePointLayer) singlePointLayer.clear();
			}
		}
	},
	
	/**
	 * Set the visibility of the graph
	 */
	setVisible: function(vis){
		this.visible = vis ? true : false;
		if (vis) 
			this.layerGroup.show();
		else 
			this.layerGroup.hide();
		if (this.legendItem) 
			this.legendItem.className = vis ? '' : HIDDEN;
	},
	
	
	
	/**
	 * Calculate the mouseover area coordinates for a given data series
	 */
	getAreaCoords: function(){
	
		var data = this.data, 
			series = this, 
			datas = [], 
			chart = this.chart, 
			inverted = chart.inverted, 
			plotWidth = chart.plotWidth, 
			plotHeight = chart.plotHeight, 
			snap = 10, 
			i = 0, 
			ret = [];
		
		
		each(this.segments, function(data, i){
			if (chart.options.xAxis.reversed) data = reverseArray(data);
			var coords = [], outlineTop = [], outlineBottom = [];
			each([outlineTop, outlineBottom], function(outline){
				var last = 0, i = 0, extreme, slice, 
					peaks = [data[0]], // add the first point at init
 					sign = outline == outlineTop ? 1 : -1, 
					intersects, 
					num, 
					x, y, x1, y1, x2, y2, dX, dY, pX, pY, l, factor, p1, p2, mA, mB, iX, iY, area;
				
				// pull out the highest and lowest peaks in slices of {snap} width,
				// push these peaks into the peaks array.
				while (data[i]) {
					if (data[i].x > data[last].x + snap || i == data.length - 1) {
						extreme = data[i];
						slice = data.slice(last, i - 1);
						each(slice, function(point){
							if (sign * point.y < sign * extreme.y) 
								extreme = point;
						});
						if (mathRound(data[last].x) < mathRound(extreme.x)) 
							peaks.push(extreme);
						
						last = i;
					}
					i++;
				}
				peaks.push(data[data.length - 1]); // the last point
				// loop through the peaks and calculate rectangles {snap} pixels
				// away from the peaks.
				for (i = 0; i < peaks.length; i++) {
				
					// clickable area
					if (i > 0) {
						// vector from last point to this
						x = peaks[i].x;
						y = peaks[i].y;
						
						
						dX = x - peaks[i - 1].x;
						dY = y - peaks[i - 1].y;
						
						
						// perpendicular vector
						pX = dY;
						pY = -dX;
						
						// length of the perpendicular vector
						l = math.sqrt(math.pow(pX, 2) + math.pow(pY, 2));
						
						// factors compared to snap
						factor = sign * snap / l;
						
						// incremental calculation of the top and bottom line
						
						// the new upper parallel vector
						x1 = mathRound(peaks[i - 1].x + factor * pX);
						y1 = mathRound(peaks[i - 1].y + factor * pY);
						x2 = mathRound(x + factor * pX);
						y2 = mathRound(y + factor * pY);
						
						// loop back until this line intersects a previous line
						if (outline[outline.length - 1] && outline[outline.length - 1][0] > x1) {
							intersects = false;
							while (!intersects) {
								p2 = outline.pop();
								p1 = outline[outline.length - 1];
								if (!p1) 
									break;
								// get intersection point
								// http://www.ultrashock.com/forums/showthread.php?t=81785
								mA = (y1 - y2) / (x1 - x2);
								mB = (p1[1] - p2[1]) / (p1[0] - p2[0]);
								
								iX = ((-mB * p1[0]) + p1[1] + (mA * x1) - y1) / (mA - mB);
								iY = (mA * (iX - x1)) + y1;
								
								if (iX > p1[0]) {
									outline.push([mathRound(iX), mathRound(iY), 1]);
									intersects = true;
								}
								
							}
						}
						else {
							if (!isNaN(x1)) 
								outline.push([x1, y1]);
						}
						if (outline[outline.length - 1] && outline[outline.length - 1][0] < x2) 
							outline.push([x2, y2]);
					}
				}
			});
			
			// area for detecting moveover
			for (i = 0; i < outlineTop.length; i++) { // top of the area
				coords.push(
					inverted ? plotWidth - outlineTop[i][1] : outlineTop[i][0], 
					inverted ? plotHeight - outlineTop[i][0] : outlineTop[i][1]
				);
				
			}
			for (i = outlineBottom.length - 1; i >= 0; i--) { // bottom of the area
				coords.push(
					inverted ? plotWidth - outlineBottom[i][1] : outlineBottom[i][0], 
					inverted ? plotHeight - outlineBottom[i][0] : outlineBottom[i][1]
				);
			}
			
			// single point: make circle
			if (!coords.length) {
				coords.push(mathRound(data[0].x), mathRound(data[0].y));
			}
			// visualize
			// console.log(coords);
			// series.stateLayers[''].drawPolyLine(coords, '#afaf00', 1);
			
			ret.push([coords.join(',')]);
		});
		return ret;
	},
	
	createArea: function(){
		var area, 
			series = this, 
			chart = series.chart, 
			coordsArray = series.getAreaCoords(), 
			imagemap = chart.tracker.imagemap, 
			firstArea = imagemap.firstChild, 
			seriesAreas = [], 
			isCircle;
			
		
		each(coordsArray, function(coords){
			isCircle = /^[0-9]+,[0-9]+$/.test(coords[0]);
			area = createElement('area', {
				shape: isCircle ? 'circle' : 'poly',
				chart: chart,
				coords: coords[0] + (isCircle ? ',10' : ''),
				onmouseover: function(){
					if (!series.visible) return;
					
					
					var hoverSeries = chart.hoverSeries;
					
					// for column/scatterplots, register that we entered a new column
					// coords[1] contains the reference to a point - if
					// no such reference is given, the area refers to 
					// a series
					chart.hoverPoint = coords[1];
					//if (/column|pie/.test(series.type)) series.entered = true;
					
					// set normal state to previous series
					if (hoverSeries && hoverSeries != series) 
						hoverSeries.setState();
					
					// bring to front	
					imagemap.insertBefore(this, imagemap.childNodes[0]);
					
					// hover this
					series.setState("hover");
					chart.hoverSeries = series;
				}
			});
			
			// insert latest on top
			if (firstArea) 
				imagemap.insertBefore(area, firstArea);
			else 
				imagemap.appendChild(area);
			seriesAreas.push(area);
		});
		series.areas = seriesAreas;
	}
}; // end Series prototype


/**
 * AreaSeries object
 */
var AreaSeries = extendClass(LineSeries, {
	type: 'area'
});

/**
 * SplineSeries object
 */
var SplineSeries = extendClass( LineSeries, {
	type: 'spline',
	/**
	 * Draw the actual spline line with interpolated values
	 * @param {Object} state
	 */
	drawLine: function(state) {
		var series = this,
			realSegments = series.segments; 
		
		// temporarily set the segments to reflect the spline
		series.segments = series.splinedata || series.getSplineData();
		
		// draw the line
		LineSeries.prototype.drawLine.apply(series, arguments);
		
		// reset the segments
		series.segments = realSegments;	
	},
	/**
	 * Get interpolated spline values
	 */
	getSplineData: function() {
		var chart = this.chart,
			//data = this.data,
			splinedata = [],
			num;
			
		each (this.segments, function(data) {
			if (chart.options.xAxis.reversed) data = reverseArray(data);
			var croppedData = [];
			
			// to save calculations, only add data within the plot
			each (data, function(point, i) {
				var nextUp = data[i+2] || data[i+1] || point;
				var nextDown = data[i-2] || data[i-1] || point;
				if (nextUp.x > 0 && nextDown.x < chart.plotWidth) {
					croppedData.push(point);
				}
			});
			
				
	    	// 3px intervals:
			if (croppedData.length > 1) {
				num = mathRound(math.max(chart.plotWidth, 
					croppedData[croppedData.length-1].clientX	- croppedData[0].clientX) / 3);
			}
	    	splinedata.push (num ? (new SplineHelper(croppedData)).get(num) : []);
		});
		return this.splinedata = splinedata;
		//return splinedata;
	}
});


/**
 * AreaSplineSeries object
 */
var AreaSplineSeries = extendClass(SplineSeries, {
	type: 'areaspline'
});

/**
 * ColumnSeries object
 */
var ColumnSeries = extendClass(LineSeries, {
	type: 'column',
	
	init: function() {
		LineSeries.prototype.init.apply(this, arguments);
		
		// record number of column series to calculate column width
		var chart = this.chart;
		if (chart.columnCount && !this.options.stacking) chart.columnCount++;
		else chart.columnCount = 1;
		this.columnNumber = chart.columnCount;
	},
	
	translate: function() {
		LineSeries.prototype.translate.apply(this);
		
		
		// calculate the width and position of each column based on 
		// the number of column series in the plot, the groupPadding
		// and the pointPadding options
		var series = this,
			options = series.options,
			data = series.data,
			chart = series.chart,
			inverted = chart.inverted,
			plotWidth = chart.plotWidth,
			plotHeight = chart.plotHeight,
			categoryWidth = data[1] ? data[1].x - data[0].x : 
				inverted ? plotHeight : plotWidth,
			groupPadding = categoryWidth * options.groupPadding,
			groupWidth = categoryWidth - 2 * groupPadding,
			pointOffsetWidth = groupWidth / chart.columnCount,
			pointPadding = pointOffsetWidth * options.pointPadding,
			pointWidth = pointOffsetWidth - 2 * pointPadding,
			columnNumber = chart.options.xAxis.reversed ? chart.columnCount - 
				series.columnNumber : series.columnNumber - 1,
			pointX = -(categoryWidth / 2) + groupPadding + columnNumber *
				pointOffsetWidth + pointPadding,
			//pointY0 = plotWidth - chart.xAxis.translate(0),
			translatedY0 = chart.yAxis.translate(0);
			
				
		// record the new values
		each (data, function(point) {
			point.x += pointX;
			point.w = pointWidth;
			point.y0 = (inverted ? plotWidth : plotHeight) - translatedY0;
			point.h = (point.yBottom || point.y0) - point.y;
		});
		
	},
	
	drawLine: function() {
	},
	
	getSymbol: function(){
	},
	
	drawPoints: function(state) {
		var series = this,
			options = series.options,
			chart = series.chart,
			plot = chart.plot,
			inverted = chart.inverted,
			data = series.data,
			state = series.state,
			layer = series.stateLayers[state];
			
		// make ready for animation
		if (series.animate) this.animate(true);
	    
		// draw the columns
		each (data, function(point) {
			h = point.h;
			if (point.y !== undefined) layer.drawRect(
				inverted ? chart.plotWidth - point.y0  : point.x,
				inverted ? chart.plotHeight - point.x - point.w : 
					(point.h >= 0 ? point.y : point.y + point.h), // for negative bars, subtract h (Opera) 
				inverted ? point.h : point.w, 
				inverted ? point.w : mathAbs(point.h), 
				options.borderColor, 
				options.borderWidth, 
				options.borderRadius, 
				series.color,
				options.shadow
			);
		});
		if (this.animate) this.animate();
	},
	

	/**
	 * Draw a single point in hover state
	 */
	drawPointState: function(point, state) {
		// local vars
		var series = this,
			chart = series.chart,		
			plot = chart.plot,
			inverted = chart.inverted,
			singlePointLayer = series.singlePointLayer; 
			
		// use one layer each series as opposed to the chartwide singlePointLayer for line-type series.
		if (!singlePointLayer) singlePointLayer = series.singlePointLayer = new Layer(
				'single-point-layer', 
				series.layerGroup.div
			);
		singlePointLayer.clear();			
			
		
		// draw the column
		if (state && this.options.states[state]) {
			var options = merge(this.options, this.options.states[state]);
			singlePointLayer.drawRect(
				inverted ? chart.plotWidth - point.y0 : point.x, 
				inverted ? chart.plotHeight - point.x - point.w : point.y, 
				inverted ? point.h : point.w, 
				inverted ? point.w : point.h, 
				options.borderColor, 
				options.borderWidth, 
				options.borderRadius, 
				Color(options.color || this.color).brighten(options.brightness).get(), 
				options.shadow		
			)
		}
	},
	
	getAreaCoords: function() {
		var areas = [],
			chart = this.chart,
			inverted = chart.inverted;
		each (this.data, function(point) {
			var x1 = inverted ? chart.plotWidth - point.y0 : point.x,
				y2 = inverted ? chart.plotHeight - point.x - point.w  : point.y,
				y1 = y2 + (inverted ? point.w : point.h),
				x2 = x1 + (inverted ? point.h : point.w);
				
			// push an array containing the coordinates and the point
			areas.push([
				map([
					x1, y1, 
					x1, y2, 
					x2, y2, 
					x2, y1
				], mathRound).join(','),
				point
			]);
			
		});
		return areas;
	},
	
	animate: function(init) {
		var series = this,
			chart = series.chart,
			inverted = chart.inverted,
			div = series.layerGroup.div,
			dataLabelsLayer = series.dataLabelsLayer;
			
		if (init) { // initialize the animation
			div.style[inverted ? 'left' : 'top'] = 
				(inverted ? -chart.plotWidth : chart.plotHeight) + PX;
			
				
		} else { // run the animation
			animate(div, this.chart.inverted ? { left: 0 } : { top: 0 });
		
			
			// delete this function to allow it only once
			this.animate = null;
		}
		
	}
});

var BarSeries = extendClass(ColumnSeries, {
	type: 'bar',
	init: function(chart) {
		chart.inverted = true;
		ColumnSeries.prototype.init.apply(this, arguments);
	}
});

var ScatterSeries = extendClass(LineSeries, {
	type: 'scatter', 
	/**
	 * Calculate the mouseover area coordinates for a given data series
	 */
	getAreaCoords: function () {

		var data = this.data,
			coords, 
			ret = [];
			
			
		each (data, function(point) {
			// create a circle for each point
			ret.push([[mathRound(point.x), mathRound(point.y)].join(','), point]);
		});
		return ret;
	}
});	

var PieSeries = extendClass(LineSeries, {
	type: 'pie',
	needsAxes: false,
	getColor: function() {
		// pie charts have a color each point
	},
	translate: function() {
		var sum = 0,
			series = this,
			cumulative = -.25, // start at top
			options = series.options,
			slicedOffset = options.slicedOffset,
			positions = options.center,
			size = options.size,
			chart = series.chart,
			data = series.data,
			circ = 2 * math.PI,
			defaultColors = chart.options.colors;
			
		// get positions - either an integer or a percentage string must be given
		positions.push(options.size);
		positions = map (positions, function(length, i) {
			return /%$/.test(length) ? 
				// i == 0: centerX, relative to width
				// i == 1: centerY, relative to height
				// i == 2: size, relative to height
				chart['plot' + (i ? 'Height' : 'Width')] * parseInt(length) / 100:
				length;
		});
					
		// get the total sum
		each (data, function(point) {
			sum += point[1];
		});
		
		each (data, function(point) {
			// set start and end angle
			point.start = cumulative * circ;
			cumulative += point[1] / sum;
			point.end = cumulative * circ;
			
			// set size and positions
			point.center = [positions[0], positions[1]];
			point.size = positions[2];
			
			// center for the sliced out slice
			var angle = (point.end + point.start) / 2;
			point.centerSliced = map([
				mathCos(angle) * slicedOffset + positions[0], 
				mathSin(angle) * slicedOffset + positions[1]
			], mathRound);
			
			// Objectify because piechart points behave like entire series of
			// other types. TODO: Consider using a point object with prototype, 
			// where options from the data series is copied into object options.
			
			// set color
			if (!point.color) point.color = defaultColors[colorCounter++];
			if (colorCounter >= defaultColors.length) colorCounter = 0;
			
			// initial visibility
			if (point.visible === undefined) point.visible = 1;
			
			// create an individual layer
			if (!point.layer) point.layer = new Layer('pie', series.layerGroup.div);
			
			// functions for the legend
			point.setState = function(state) {
				series.drawPointState(point, state);
			}
			point.setVisible = function(vis) {
				var fn = vis ? 'show' : 'hide',
					legendItem = point.legendItem;
				point.visible = vis;
				point.layer[fn](); // show or hide
				if (legendItem) legendItem.className = vis ? '' : HIDDEN;
			}
		});
		
		this.setTooltipPoints();
	},
	
	/**
	 * Render the graph and markers
	 */
	render: function() {
		
		if (!this.pointsDrawn) this.drawPoints();
		this.drawDataLabels();
	},
	drawPoints: function(state) {
		var series = this;
		
		// draw the slices
		each (this.data, function(point) {
			series.drawPoint(point, point.layer.getCtx(), point.color);
			
			//if (point.sliced) this.slice(point);		
		});
		
		series.pointsDrawn = true;
	},
	
	getSymbol: function(){
	},
	

	/**
	 * Draw a single point in hover state
	 */
	drawPointState: function(point, state) {
		var series = this,
			seriesOptions = series.options,
			stateLayer;
			
		if (point) {

			// create a special state layer nested in this point's main layer
			stateLayer = point.stateLayer;
			if (!stateLayer) 
				stateLayer = point.stateLayer = new Layer('state-layer', point.layer.div);
			stateLayer.clear();
			
			
			// draw the point
			if (state && series.options.states[state]) {
				var options = merge(seriesOptions, seriesOptions.states[state]);
				this.drawPoint(
					point, 
					stateLayer.getCtx(), 
					options.color || point.color, 
					options.brightness
				);
			}
			
		}
		// clear the old point on register the new one
		if (series.hoverPoint) series.hoverPoint.stateLayer.clear();
		series.hoverPoint = point;
	},
	
	drawPoint: function(point, ctx, color, brightness) {
		var center = point.sliced ? point.centerSliced : point.center,
			centerX = center[0],
			centerY = center[1],
			size = point.size;

		ctx.fillStyle = Color(color).brighten(brightness).get(ctx);
		ctx.beginPath();
		ctx.moveTo(centerX, centerY);
		ctx.arc(centerX, centerY, size / 2, point.start, point.end, false);
		ctx.lineTo(centerX, centerY);
		ctx.closePath();
		ctx.fill();

	},
	/**
	 * Pull the slice out from the pie
	 * @param {Object} point
	 */
	/*slice: function(point) {
		var centerSliced = point.centerSliced;
		setStyles(point.layer, {
			left: centerSliced[0] + PX,
			top: centerSliced[1] + PX
		});
	},*/
	
	getAreaCoords: function() {
		var areas = [];
		var series = this;
		each (this.data, function(point) {
			var centerX = point.center[0],
				centerY = point.center[1],
				radius = point.size / 2,
				start = point.start,
				end = point.end,
				coords = [];
				
			// start building the coordinates from the start point
			// with .25 radians (~15 degrees) increments the coordinates
			for (var angle = start; angle; angle += .25) {
				if (angle >= end) angle = end;
				coords = coords.concat([
					centerX + mathCos(angle) * radius,
					centerY + mathSin(angle) * radius
				])
				if (angle >= end) break;
			}
			
			// wrap it up with the center point		
			coords = coords.concat([
				centerX, centerY
			]);
						
			// set tooltip position in the center of the sector
			point.tooltipPos = [
				centerX + 2 * mathCos((start + end) / 2) * radius / 3,
				centerY + 2 * mathSin((start + end) / 2) * radius / 3
			];
			
			// push an array containing the coordinates and the point
			areas.push([
				map(coords, mathRound).join(','),
				point
			])
			
		});
		return areas;
	}
});

/**
 * Ported from JPGraph
 */
function SplineHelper (data) {
	var xdata = [];
	var ydata = [];
	for (var i = 0; i < data.length; i++) {
		xdata[i] = data[i].x;
		ydata[i] = data[i].y;
	}
	this.xdata = xdata;
	this.ydata = ydata;
	var delta = [];
	this.y2 = [];

	var n = ydata.length;
	this.n = n;

	// Natural spline 2:derivate == 0 at endpoints
	this.y2[0]    = 0.0;
	this.y2[n-1] = 0.0;
	delta[0] = 0.0;

	// Calculate 2:nd derivate
	for(var i=1; i < n-1; i++) {
	    var d = (xdata[i+1]-xdata[i-1]);
	    /*if( d == 0  ) {
			alert ('Invalid input data for spline. Two or more consecutive input X-values are equal. Each input X-value must differ since from a mathematical point of view it must be a one-to-one mapping, i.e. each X-value must correspond to exactly one Y-value.');
	    }*/
	    var s = (xdata[i]-xdata[i-1])/d;
	    var p = s*this.y2[i-1]+2.0;
	    this.y2[i] = (s-1.0)/p;
	    delta[i] = (ydata[i+1]-ydata[i])/(xdata[i+1]-xdata[i]) -
		         (ydata[i]-ydata[i-1])/(xdata[i]-xdata[i-1]);
	    delta[i] = (6.0*delta[i]/(xdata[i+1]-xdata[i-1])-s*delta[i-1])/p;
	}

	// Backward substitution
	for(var j=n-2; j >= 0; j-- ) {
	    this.y2[j] = this.y2[j]*this.y2[j+1] + delta[j];
	}
};
SplineHelper.prototype = {
// Return the two new data vectors
get: function(num) {
	if (!num) num = 50;
	var n = this.n ;
	var step = (this.xdata[n-1]-this.xdata[0]) / (num-1);
	var xnew=[];
	var ynew=[];
	xnew[0] = this.xdata[0];
	ynew[0] = this.ydata[0];
	var data = [{ x: xnew[0], y: ynew[0] }];//[[xnew[0], ynew[0]]];

	for(var j = 1; j < num; j++ ) {
	    xnew[j] = xnew[0]+j*step;
	    ynew[j] = this.interpolate(xnew[j]);
	    data[j] = { x: xnew[j], y: ynew[j] };//[xnew[j], ynew[j]];
	}

	return data;
},

// Return a single interpolated Y-value from an x value
interpolate: function(xpoint) {
	var max = this.n-1;
	var min = 0;

	// Binary search to find interval
	while( max-min > 1 ) {
	    var k = (max+min) / 2;
		if( this.xdata[mathFloor(k)] > xpoint )
			max=k;
	    else
			min=k;
	}
	var intMax = mathFloor(max), intMin = mathFloor(min);

	// Each interval is interpolated by a 3:degree polynom function
	var h = this.xdata[intMax]-this.xdata[intMin];
	/*if( h == 0  ) {
	    alert('Invalid input data for spline. Two or more consecutive input X-values are equal. Each input X-value must differ since from a mathematical point of view it must be a one-to-one mapping, i.e. each X-value must correspond to exactly one Y-value.');
	}*/


	var a = (this.xdata[intMax]-xpoint)/h;
	var b = (xpoint-this.xdata[intMin])/h;
	return a*this.ydata[intMin]+b*this.ydata[intMax]+
	     ((a*a*a-a)*this.y2[intMin]+(b*b*b-b)*this.y2[intMax])*(h*h)/6.0;
}

};

// global variables
Highcharts = {
	'numberFormat': numberFormat,
	'dateFormat': dateFormat,
	//'defaultOptions': defaultOptions,
	'setOptions': setOptions,
	'Chart': Chart
}
})();
