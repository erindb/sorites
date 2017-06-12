var structure = [
	"intro",
	"instructions",
	"trial*",
	"questionnaire",
	"thanks"
];

var data = {
	phrasing: "conditional",
    responses: []
};

var make_sorites_stack = function(cond) {
	var objects = ["laptop", "watch", "coffee maker", "sweater", "headphones"];
	var values = {
		"laptop": [350, 600, 900, 1250, 1850],
		"watch": [100, 250, 450, 900, 2000],
		"coffee maker": [24, 52, 84, 124, 188],
		"sweater": [18, 36, 57, 87, 150],
		"headphones": [24, 60, 96, 144, 234]
	};
	var epsilons = {
		"laptop": [18.50, 185.0, 925.0, 1295.0, 1850],
		"watch": [24.00, 240.0, 1200.0, 1680.0, 2400],
		"coffee maker": [2.00, 20.0, 100.0, 140.0, 200],
		"sweater": [1.71, 17.1, 85.5, 119.7, 171],
		"headphones": [2.58, 25.8, 129.0, 180.6, 258]
	};
	var qtypes = ["inductive", "concrete"];
	var stack = [];
	for (var o=0; o<objects.length; o++) {
		for (var level=0; level<5; level++) {
			for (var q=0; q<2; q++) {
					var qtype = qtypes[q];
					var obj = objects[o];
				stack.push({
					qtype: qtype,
					object: obj,
					level: level,
					dollar_amount: qtype == "inductive"? epsilons[obj][level] : values[obj][level],
					phrasing: data.phrasing
				});
			}
		}
	}
	return _.shuffle(stack);
}

var stacks = {
	trial: make_sorites_stack(data.phrasing)
}

var length_in_minutes = 8;

var pretty_price = function(price) {
	var price_string = price.toString();
	if (price * 100 % 100 < 0.001) {
		price_string += ".00";
	} else if (price * 100 % 10 < 0.001) {
		price_string += "0";
	}
	return "$" + price_string;
}

var slide_info = {
	trial: {
		current_response_data: null,
		start: function(stack_element) {
			slide_info.trial.current_response_data = _.clone(stack_element);
			console.log(stack_element.dollar_amount);
			var make_premise = function(qtype, dollarAmt, obj, phrasing) {
				var qtype = stack_element.qtype;
				var dollar_amount = pretty_price(stack_element.dollar_amount);
				var obj = stack_element.object;
				var phrasing = stack_element.phrasing;
				var make_inductive_premise = function(dollar_amount, obj, phrasing) {
					var singular_object = obj == "headphones" ? "pair of headphones" : obj;
					var relative_clause = "A " + singular_object + " that costs " + dollar_amount +
					                      " less than an expensive " + singular_object +
					                      " is also expensive.";
					var conditional = "If a " + singular_object + " is expensive, then another " +
					                  singular_object + " that costs " + dollar_amount +
					                  " less is also expensive.";
					if (phrasing == "relative_clause") {
						return relative_clause;
					} else if (phrasing == "conditional") {
						return conditional;
					} else {
						console.log("error 10");
					}
				}
				var make_concrete_premise = function(dollar_amount, obj) {
					var singular_object = obj == "headphones" ? "pair of headphones" : obj;
					return "A " + singular_object + " that costs " + dollar_amount + " is expensive.";
				}
				if (qtype == "inductive") {
					return make_inductive_premise(dollar_amount, obj, phrasing);
				} else if (qtype == "concrete") {
					return make_concrete_premise(dollar_amount, obj);
				} else {
					console.log("error 11");
				}
			}
			$("#sentence").html(make_premise(stack_element));
		},
		next: function() {
			var response = $("input[name=rating]:checked").val();
			if (response == null) {
				alert("Please provide a response. Thank you!");
			} else {
				slide_info.trial.current_response_data.response = response;
				data.responses.push(slide_info.trial.current_response_data);
	        	$("input[name=rating]").prop("checked", false);
				slide_info.trial.current_response_data = null;
				cont();
			}
		}
	}
}