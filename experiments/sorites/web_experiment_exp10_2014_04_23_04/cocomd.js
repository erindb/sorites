var state;
var length_in_questions = -1;
var startT;

var show_slide = function(slide_name) {
	$(".slide").hide();
	$("#" + slide_name).show();
}
var is_stack = function(slide_name) {
	return /.*\*/.test(slide_name);
}
var get_name = function(slide_name) {
	return /([^*]*)\*?/.exec(slide_name)[1];
}

var run_slide = function(stack_element) {
	show_slide(state.slide_name);
	if (slide_info[state.slide_name]) {
		slide_info[state.slide_name].start(stack_element);
	}
}

var get_next = function(slide_name) {
	if (slide_info[slide_name]) {
		var next_function = slide_info[slide_name].next ? slide_info[slide_name].next : cont;
	} else {
		var next_function = cont;
	}
	return next_function;
}

var cont = function() {
	if (state.is_stack & !state.end_of_stack) {
		var stack = stacks[state.slide_name];
		var stack_element = stack[state.within_stack_presented]
		var new_within_stack_presented = state.within_stack_presented + 1;
		var new_total_presented = state.total_presented + 1;

		state = {
			slide_name: state.slide_name,
			slide_type_number: state.slide_type_number,
			within_stack_presented: new_within_stack_presented,
			total_presented: new_total_presented,
			next: get_next(state.slide_name),
			is_stack: true,
			end_of_stack: new_within_stack_presented >= (stack.length)
		}
	}
	else {
		var old_slide_type_number = state.slide_type_number;
		var new_slide_type_number = old_slide_type_number + 1;
		var new_slide_structure_tag = structure[new_slide_type_number];
		var new_total_presented = state.total_presented + 1;
		var new_slide_name = get_name(new_slide_structure_tag);
		var stack_element = is_stack(new_slide_structure_tag) ? stacks[new_slide_name][0] : null;
		var new_is_stack = is_stack(new_slide_structure_tag);

		state = {
			slide_name: new_slide_name,
			slide_type_number: new_slide_type_number,
			within_stack_presented: is_stack(new_slide_structure_tag) ? 1 : null,
			total_presented: new_total_presented,
			is_stack: new_is_stack,
			next: get_next(new_slide_name),
			end_of_stack: new_is_stack ? false : null
		}
	}
	$('.bar').css('width', ( (state.total_presented / length_in_questions)*100 + "%"));
	run_slide(stack_element)
}

var get_length_in_questions = function() {
	for (var i=0; i<structure.length; i++) {
		if (is_stack(structure[i])) {
			//might need to fix this if it's a list of lists
			length_in_questions = length_in_questions + stacks[get_name(structure[i])].length;
		} else {
			length_in_questions ++;
		}
	}
}

var leap = function(where_to_go) {
	var old_slide_type_number = state.slide_type_number;
	if (typeof where_to_go == "string") {
		if (structure.indexOf(where_to_go) >= 0) {
			var new_slide_type_number = structure.indexOf(where_to_go);
		} else if (structure.indexOf(where_to_go + "*")>=0) {
			var new_slide_type_number = structure.indexOf(where_to_go + "*");
		} else {
			var new_slide_type_number = structure.length -1;
		}
		var n = new_slide_type_number - old_slide_type_number;
		console.log(where_to_go);
		console.log(new_slide_type_number);
	} else {
		var n = where_to_go ? where_to_go : 1;
		var new_slide_type_number = Math.min(old_slide_type_number + n, structure.length -1);
	}
	var new_slide_structure_tag = structure[new_slide_type_number];
	var new_total_presented = state.total_presented;
	var new_slide_name = get_name(new_slide_structure_tag);
	var stack_element = is_stack(new_slide_structure_tag) ? stacks[new_slide_name][0] : null;

	if (state.is_stack) {
		new_total_presented = new_total_presented + stacks[state.slide_name].length - state.within_stack_presented;
	} else {
		new_total_presented++;
	}
	for (i=old_slide_type_number+1; i<new_slide_type_number; i++) {
		if (is_stack(structure[i])) {
			new_total_presented = new_total_presented + stacks[get_name(structure[i])].lengthl;
		} else {
			new_total_presented++;
		}
	}

	state = {
		slide_name: new_slide_name,
		slide_type_number: new_slide_type_number,
		within_stack_presented: is_stack(new_slide_structure_tag) ? 0 : null,
		total_presented: new_total_presented,
		is_stack: is_stack(new_slide_structure_tag),
		next: get_next(new_slide_name),
		end_of_stack: is_stack(new_slide_structure_tag) ? false : null
	}

	$('.bar').css('width', ( (state.total_presented / length_in_questions)*100 + "%"));
	run_slide(stack_element);
}

$(document).ready(function() {
	state = {
		slide_name: "intro",
		slide_type_number: 0,
		within_stack_presented: null,
		total_presented: 0,
		is_stack: false,
		end_of_stack: null,
		next: cont
	};
	startT = Date.now();
	get_length_in_questions();
	$("#length_in_minutes").html(length_in_minutes);
	$("#length_in_questions").html(length_in_questions);

	slide_info.questionnaire = {
		start: function() {
			data.time_in_minutes = (Date.now() - startT)/60000;
			data.system = {
				Browser : BrowserDetect.browser,
				OS : BrowserDetect.OS,
				screenH: screen.height,
				// screenUH: exp.height,
				screenW: screen.width//,
				// screenUW: exp.width
		    };
		}
	};
	slide_info.thanks = {
		start: function() {
		    data.subj_data = {
				language : $("#language").val(),
				enjoyment : $("#enjoyment").val(),
				asses : $('input[name="assess"]:checked').val(),
				age : $("#age").val(),
				gender : $("#gender").val(),
				education : $("#education").val(),
				comments : $("#comments").val(),
			};
			setTimeout(function() {
				turk.submit(data);
				$("#submitting").html("Thank you for your time!!");
			}, 1000);
		}
	};

	run_slide();
});