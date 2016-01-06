function caps(a) {return a.substring(0,1).toUpperCase() + a.substring(1,a.length);}
function uniform(a, b) { return ( (Math.random()*(b-a))+a ); }
function showSlide(id) { $(".slide").hide(); $("#"+id).show(); }
function shuffle(v) { newarray = v.slice(0);for(var j, x, i = newarray.length; i; j = parseInt(Math.random() * i), x = newarray[--i], newarray[i] = newarray[j], newarray[j] = x);return newarray;} // non-destructive.
function sample(v) { return(shuffle(v)[0]) }

var items = shuffle(["watch", "laptop", "coffee maker", "headphones", "sweater"]);
//var buyerGenders = shuffle(["boys", "girls", "both"]);
var genderCond = "boys";//buyerGenders[0];
if (genderCond == "boys") {
  var buyers = shuffle([
    "Aaron", "Adam", "Adrian", "Aiden", "Alan", "Albert", "Alex", "Alfred",
    "Alvin", "Andrew", "Anthony", "Arnold", "Arthur", "Austin", "Ayden",
    "Barry", "Ben", "Bentley", "Bernard", "Bill", "Blake", "Bob", "Bradley",
    "Brandon", "Brayden", "Brent", "Brett", "Brian", "Brody", "Bruce", "Bryson",
    "Byron", "Caleb", "Calvin", "Cameron", "Carl", "Carson", "Carter", "Casey",
    "Chad", "Charlie", "Chase", "Chester", "Chris", "Clarence", "Claude",
    "Clayton", "Clifford", "Clifton", "Clinton", "Clyde", "Cody", "Colin",
    "Colton", "Connor", "Cooper", "Cory", "Craig", "Curtis", "Dale", "Damian",
    "Danny", "Darren", "Daryl", "Dave", "Dennis", "Derrick", "Dominic",
    "Donald", "Dorian", "Douglas", "Dustin", "Dwayne", "Dwight", "Dylan",
    "Easton", "Eddie", "Edgar", "Edwin", "Eli", "Elijah", "Elliott", "Eric",
    "Ernest", "Ethan", "Eugene", "Evan", "Everett", "Felix", "Floyd", "Frank",
    "Fred", "Gabriel", "Gary", "Gavin", "Gene", "George", "Gerald", "Gilbert",
    "Glen", "Gordon", "Grayson", "Greg", "Harold", "Harry", "Harvey", "Hector",
    "Henry", "Herbert", "Herman", "Howard", "Hudson", "Hugh", "Hunter", "Ian",
    "Isaac", "Isaiah", "Ivan", "Jace", "Jack", "Jackson", "Jacob", "James",
    "Jared", "Jason", "Jaxon", "Jay", "Jayden", "Jeff", "Jeremiah", "Jeremy",
    "Jerome", "Jerry", "Jesse", "Jim", "Joe", "Joel", "John", "Jonathan",
    "Jordan", "Jose", "Joseph", "Josh", "Joshua", "Josiah", "Juan", "Julian",
    "Julius", "Justin", "Kayden", "Keith", "Ken", "Kent", "Kevin", "Kurt",
    "Kyle", "Lance", "Landon", "Larry", "Lawrence", "Lee", "Leo", "Leon",
    "Leonard", "Leroy", "Lester", "Levi", "Liam", "Lloyd", "Logan", "Lonnie",
    "Louis", "Lucas", "Luis", "Luke", "Mark", "Marshall", "Martin", "Marvin",
    "Mason", "Matthew", "Maurice", "Max", "Melvin", "Michael", "Milton",
    "Mitchell", "Morris", "Nathan", "Nathaniel", "Neil", "Nelson", "Nicholas",
    "Noah", "Nolan", "Norman", "Oliver", "Oscar", "Owen", "Parker", "Patrick",
    "Paul", "Perry", "Peter", "Phillip", "Quentin", "Ralph", "Randy", "Ray",
    "Raymond", "Rick", "Robert", "Rodney", "Roger", "Roland", "Ron", "Ross",
    "Roy", "Ruben", "Russell", "Ryan", "Ryder", "Sam", "Samuel", "Scott",
    "Sean", "Sebastian", "Seth", "Shane", "Stanley", "Steven", "Ted",
    "Terrence", "Theodore", "Thomas", "Tim", "Todd", "Tony", "Travis",
    "Tristan", "Troy", "Tyler", "Tyrone", "Uri", "Vernon", "Victor", "Vincent",
    "Wade", "Wallace", "Walter", "Warren", "Wayne", "Wesley", "Willard",
    "William", "Willie", "Wyatt", "Xavier", "Yakov", "Zachary"
  ]);
} else if (genderCond == "girls") {
  var buyers = shuffle(["Ann", "Beth", "Caitlyn", "Danielle", "Emma"]);
} else {
  var buyers = shuffle([
    "Alan", "Bob", "Calvin", "Dan", "Evan", "Ann", "Beth", "Caitlyn",
    "Danielle", "Emma"
  ]);
}
var colors = shuffle(["DF0101", "31B404", "0404B4", "FF8000", "B404AE"]);
var color = {}
for (var i=0; i<items.length; i++) {
  color[items[i]] = colors[i];}

var conds = ["original", "split"];
var cond = sample(conds);

var expensive = sample([true, false]);

pronoun = {"watch":"It was",
           "laptop":"It was",
           "coffee maker":"It was",
           "headphones":"They were",
           "sweater":"It was"};

stepLength = {
  "watch":300,
  "laptop":500,
  "coffee maker":50,
  "headphones":50,
  "sweater":50
}
maximum = {
  "watch":3000,
  "laptop":3000,
  "coffee maker":300,
  "headphones":400,
  "sweater":300
}
function nBins(item) {
  return Math.ceil(maximum[item] / stepLength[item]);
}

var plural = {"watch":"watches",
              "laptop":"laptops",
              "coffee maker":"coffee makers",
              "headphones":"headphones",
              "sweater":"sweaters"}


var nClicks = 0;
for (var i=0; i<items.length; i++) {
  nClicks += nBins(items[i]);
}
var nComplete = 0;
var nQs;
if (cond == "original") {
  nQs = items.length;
} else if (cond == "split") {
  nQs = nClicks;
  var splitItems = []
  var splitLower = []
  var splitHigher = []
  for (var i=0; i<items.length; i++) {
    for (var j=0; j<nBins(items[i]); j++) {
      var lowPrice = j*stepLength[items[i]];
      var highPrice;
      if (j == nBins(items[i]) - 1) {
        highPrice = "infty";
      } else {
        highPrice = (j+1)*stepLength[items[i]];
      }
      splitItems.push(items[i]);
      splitLower.push(lowPrice);
      splitHigher.push(highPrice);
    }
  }
  var indices = []
  for (var i=0; i<nQs; i++) {
    indices.push(i);
  }
  var myIndices = shuffle(indices);
} else {
  alert("ERROR0: " + cond + " is not a condition I know how to handle.")
}

var startTime;

// labels for making sliders and changing css, etc. 
function sliderLabel(i) {
  return("slider" + i.toString());
}

function showSlide(id) { $(".slide").hide(); $("#"+id).show(); }

$(document).ready(function() {
  showSlide("consent");
  startTime = Date.now();
  $("#mustaccept").hide();
  $("#targetError").hide();
  $("#trial-num").html("0");
  $(".tot-num").html(nQs);
  $(".nBins").html(nBins);
});

var experiment = {
  data: {version:"jan30",
         cond:cond,
         expensive:expensive},
  
  instructions: function() {
    if (turk.previewMode) {
      $("#instructions #mustaccept").show();
    } else {
      showSlide("instructions");
      $("#begin").click(function() { experiment.trial(0); })
    }
  },
  
  questionaire: function() {
    //disable return key
    $(document).keypress( function(event){
     if (event.which == '13') {
        event.preventDefault();
      }
    });
    //progress bar complete
    $('.bar').css('width', ( "100%"));
    showSlide("questionaire");
    $("#formsubmit").click(function() {
      rawResponse = $("#questionaireform").serialize();
      pieces = rawResponse.split("&");
      var age = pieces[0].split("=")[1];
      var lang = pieces[1].split("=")[1];
      var comments = pieces[2].split("=")[1];
      if (lang.length > 0) {
        experiment.data["language"] = lang;
        experiment.data["comments"] = comments;
        experiment.data["age"] = age;
        var endTime = Date.now();
        experiment.data["duration"] = endTime - startTime;
        showSlide("finished");
        setTimeout(function() { turk.submit(experiment.data) }, 1000);
      }
    });
  },

  trial: function(qNumber) {
    if (cond == "original") {
      experiment.originalTrial(qNumber)
    } else if (cond == "split") {
      experiment.splitTrial(qNumber)
    } else {
      alert("ERROR1: " + cond + " isn't a condition i'm prepared to run!!!!")
    }
  },

  splitTrial: function(qNumber) {
    $('.bar').css('width', ( (nComplete / nClicks)*100 + "%"));
    showSlide("trial");

    var myIndex = myIndices[qNumber];
    var item = splitItems[myIndex];
    var lowPrice = splitLower[myIndex];
    var highPrice = splitHigher[myIndex];
    var buyer = buyers[myIndex];

    var expensiveSentence;
    if (expensive) {
      expensiveSentence = pronoun[item] + " <b><i>expensive</i></b>.";
    } else {
      expensiveSentence = "";
    }

    $("#all-that-jazz").html(
      '<p id="statement">' + buyer + ' bought ' +
      ' <span class="article">{{}}</span> new <font color="' +
      color[item] + '"><i><b>' + item +
      '</b></i></font>. ' + expensiveSentence +
      '</p><p id="question">Please rate how likely it is ' +
      'that the cost of the ' + item +
      ' is within the range shown below the slider.</p>' +
      '<div align="center"><table><tbody><tr id="sliders"></tr>' +
      '<tr><td height="72">Very Likely</td></tr>' +
      '<tr><td height="72">Neutral</td></tr>' +
      '<tr><td height="72">Not Very Likely</td></tr>' +
      '<tr><td height="72">Extremely Unlikely</td></tr>' +
      '<tr id="prices"></tr></tbody></table></div><br/>'
    );

    if (item == "headphones") {
      $(".article").html("");
    } else {
      $(".article").html("a");
    }

    var firstColWidth = 150;
    var otherColWidth = 100;

    sliderCells = '<td height="72">Extremely Likely</td>' +
                  '<td rowspan="5" width="' + otherColWidth +
                  '" align="center"><div class="slider" id="slider">' +
                  '</div></td>';

    var priceCells = '<td width="' + firstColWidth + '"></td>';
    if (highPrice == "infty") {
      priceCells += '<td align="center" width="' + otherColWidth +
                    '"><b>more than $' + lowPrice + '</b></td>';
    } else {
      priceCells += '<td align="center" width="' + otherColWidth + '"><b>$' +
                    lowPrice + '-$' + highPrice + '</b></td>';
    }

    $("#sliders").html(sliderCells);
    $("#prices").html(priceCells);

    var trialData = {buyer:buyer,
                     item:item,
                     //max:maximum[item],
                     lower:lowPrice,
                     color:color[item],
                     upper:highPrice};
    var nResponses = 0;

    $("#slider").attr({"width":"12px",
                       "height":"360px",
                       "position":"relative",
                       "margin":"5px"});
    $("#slider .ui-slider-handle").attr({"background": "#FAFAFA"});
    $('#slider').slider({
      animate: true,
      orientation: "vertical",
      max: 1 , min: 0, step: 0.01, value: 0.5,
      slide: function() {
        $('#slider .ui-slider-handle').css({
           "background":"#E0F5FF",
           "border-color": "#001F29"
        });
      },
      change: function(value) {
        $('#slider').css({"background":"#99D6EB"});
        $('#slider .ui-slider-handle').css({
          "background":"#667D94",
          "border-color": "#001F29" });
        if (trialData.response == null) {
          nResponses++;
          nComplete++;
          $('.bar').css('width', ( (nComplete / nClicks)*100 + "%"));
        }
        trialData.response = $("#slider").slider("value");
      }
    });

    $("#continue").click(function() {
      if (nResponses < 1) {
        $("#targetError").show();
      } else {
        $("#continue").unbind("click");
        $("#targetError").hide();
        experiment.data[qNumber.toString()] = trialData;
        if (qNumber + 1 < nQs) {
          experiment.trial(qNumber+1);
        } else {
          experiment.questionaire();
        }
      }
    })
  },
  
  originalTrial: function(qNumber) {
    $('.bar').css('width', ( (nComplete / nClicks)*100 + "%"));
    showSlide("trial");

    var item = items[qNumber];
    var buyer = buyers[qNumber];

    var nRows = Math.ceil(nBins(item) / 10);
    var allthatjazz = '';
    var hr;
    if (nRows > 1) {
      hr = '<hr/>'
    } else {
      hr = ''
    }

    var expensiveSentence;
    if (expensive) {
      expensiveSentence = pronoun[item] + " <b><i>expensive</i></b>.";
    } else {
      expensiveSentence = "";
    }

    for (var i=0; i<nRows; i++) {
      allthatjazz += '<p id="statement"><span class="buyer">{{}}</span> bought ' +
                     ' <span class="article">{{}}</span> new <font color="' +
                     color[item] + '"><i><b>' + item +
                     '</b></i></font>. ' + expensiveSentence +
                     '</p><p id="question">Please rate how likely it is that the cost of the ' +
                     '<span class="item">{{}}</span> is within each of the following ranges.</p>' +
                     '<div align="center">' +
                     '<table><tbody><tr id="sliders' + i + '"></tr>' +
                     '<tr><td height="72">Very Likely</td></tr>' +
                     '<tr><td height="72">Neutral</td></tr>' +
                     '<tr><td height="72">Not Very Likely</td></tr>' +
                     '<tr><td height="72">Extremely Unlikely</td></tr>' +
                     '<tr id="prices' + i + '"></tr></tbody></table>' +
                     hr + '</div>'
    }
    $("#all-that-jazz").html(allthatjazz);

    $(".item").html(item);
    $(".buyer").html(buyer);
    if (item == "headphones") {
      $(".article").html("");
    } else {
      $(".article").html("a");
    }

    var firstColWidth = 150;
    var otherColWidth = 100;

    var lowers = [];
    var uppers = [];

    var sliderCells = ""
    var priceCells = ""
    for (var i=0; i<nBins(item); i++) {
      if (i % 10 == 0) {
        sliderCells = ""
        priceCells = ""
      }
      sliderCells += '<td rowspan="5" width="' + otherColWidth + '" align="center"><div class="slider" id="' + sliderLabel(i) + '"></div></td>';
      if (i<(nBins(item)-1)) {
        var lowPrice = i*stepLength[item];
        var highPrice = (i+1)*stepLength[item];
        lowers.push(lowPrice);
        uppers.push(highPrice);
        priceCells += '<td align="center" width="' + otherColWidth + '"><b>$' + lowPrice + '-$' + highPrice + '</b></td>';
      } else {
        var lowPrice = i*stepLength[item];
        lowers.push(lowPrice);
        uppers.push("infty");
        priceCells += '<td align="center" width="' + otherColWidth + '"><b>more than $' + lowPrice + '</b></td>';
      }
      if ((i % 10) == 9) {
        $("#sliders" + Math.floor(i/10)).html('<td height="80" width="' + firstColWidth + '">Extremely Likely</td>' + sliderCells);
        $("#prices" + Math.floor(i/10)).html('<td width="' + firstColWidth + '"></td>' + priceCells);
      }
    }
    if ((i % 10) != 0) {
      $("#sliders" + Math.floor(i/10)).html('<td height="80" width="' + firstColWidth + '">Extremely Likely</td>' + sliderCells);
      $("#prices" + Math.floor(i/10)).html('<td width="' + firstColWidth + '"></td>' + priceCells);
    }

    var trialData = {buyer:buyer,
                     item:item,
                     //max:maximum[item],
                     lowers:lowers,
                     uppers:uppers,
                     color:color[item],
                     responses:[]};
    var nResponses = 0;

    function changeCreator(i) {
      return function(value) {
        $('#' + sliderLabel(i)).css({"background":"#99D6EB"});
        $('#' + sliderLabel(i) + ' .ui-slider-handle').css({
          "background":"#667D94",
          "border-color": "#001F29" });
        if (trialData.responses[i] == null) {
          nResponses++;
          nComplete++;
          $('.bar').css('width', ( (nComplete / nClicks)*100 + "%"));
        }
        trialData.responses[i.toString()] = $("#"+sliderLabel(i)).slider("value");
      } 
    }

    function slideCreator(i) {
      return function() {
        $('#' + sliderLabel(i) + ' .ui-slider-handle').css({
           "background":"#E0F5FF",
           "border-color": "#001F29"
        });
      }
    }

    for (var i=0; i<nBins(item); i++) {
      $("#" + sliderLabel(i)).attr({"width":"12px", "height":"360px", "position":"relative", "margin":"5px"});
      $("#" + sliderLabel(i) + " .ui-slider-handle").attr({"background": "#FAFAFA"});
      $('#' + sliderLabel(i)).slider({
        animate: true,
        orientation: "vertical",
        max: 1 , min: 0, step: 0.01, value: 0.5,
        slide: slideCreator(i),
        change: changeCreator(i)
      });
    }

    $("#continue").click(function() {
      if (nResponses < nBins(item)) {
        $("#targetError").show();
      } else {
        $("#continue").unbind("click");
        $("#targetError").hide();
        experiment.data[qNumber.toString()] = trialData;
        if (qNumber + 1 < nQs) {
          experiment.trial(qNumber+1);
        } else {
          experiment.questionaire();
        }
      }
    })
  }
};
