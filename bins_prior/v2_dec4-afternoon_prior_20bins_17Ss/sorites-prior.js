function caps(a) {return a.substring(0,1).toUpperCase() + a.substring(1,a.length);}
function uniform(a, b) { return ( (Math.random()*(b-a))+a ); }
function showSlide(id) { $(".slide").hide(); $("#"+id).show(); }
function shuffle(v) { newarray = v.slice(0);for(var j, x, i = newarray.length; i; j = parseInt(Math.random() * i), x = newarray[--i], newarray[i] = newarray[j], newarray[j] = x);return newarray;} // non-destructive.
function myRound(rawPrice, roundToNearest) { return Math.round(rawPrice/roundToNearest)*roundToNearest; }

var items = shuffle(["watch", "laptop", "coffee maker", "headphones", "sweater"]);
var buyerGenders = shuffle(["boys", "girls", "both"]);
var cond = buyerGenders[0];
if (cond == "boys") {
  var buyers = shuffle(["Alan", "Bob", "Calvin", "Dan", "Evan"]);
} else if (cond == "girls") {
  var buyers = shuffle(["Ann", "Beth", "Caitlyn", "Danielle", "Emma"]);
} else {
  var buyers = shuffle(["Alan", "Bob", "Calvin", "Dan", "Evan", "Ann", "Beth", "Caitlyn", "Danielle", "Emma"]);
}

var plural = {"watch":"watches",
              "laptop":"laptops",
              "coffee maker":"coffee makers",
              "headphones":"headphones",
              "sweater":"sweaters"}

/*Using all of our prior types so far, where is a point where before this there's significant probability mass and after it there's not?
prior types:
ebay: scraped ebay
amazon: scraped amazon
original: asking people for prices of a given item - unconstrained
interval: asking people for prices of a given item - constrained by "has to be above/below $X"
justine's: asking people for probability of seeing a watch at a given price - very course-grained and only watches and laptops
*/
var maxes = {
             "coffee maker":400, //or 800? the interval expt went out that far...
             "headphones":300, //or 800? the interval expt went out that far...
             "laptop":5000, //justine's prior elicitation for hyperbole went out to 10,000
             "sweater":400, //no measure goes past this, most go to zero at around this point.
             "watch":500 //justine's went out to 10,000. interval went out to 2000. everything else went down to zero at around 500.
            };

//maybe it would make sense to round different items differently:
var roundToNearest = {"coffee maker":10, "headphones":10, "laptop":10, "sweater":10, "watch":10};

var nProbQs = items.length;
var nMaxQs = items.length;
var nQs = nProbQs + nMaxQs;
var nBins = 20;
var nClicks = nProbQs*nBins + nMaxQs;

var startTime;

// labels for making sliders and changing css, etc. 
var sliderLabel = []
for (var i=0; i<nBins; i++) {
  sliderLabel[i] = "slider" + i.toString();
}

function showSlide(id) { $(".slide").hide(); $("#"+id).show(); }

$(document).ready(function() {
  showSlide("consent");
  startTime = Date.now();
  $("#mustaccept").hide();
  $("#targetError").hide();
  $("#trial-num").html("0");
  $(".tot-num").html(nQs);
  $(".prob-num").html(nProbQs);
  $(".nBins").html(nBins);
});

var experiment = {
  data: {cond:cond},
  
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
    $('.bar').css('width', ( ((qNumber*nBins) / nClicks)*100 + "%"));
    showSlide("trial");

    var item = items[qNumber];
    var buyer = buyers[qNumber];
    $(".item").html(item);
    $(".buyer").html(buyer);
    if (item == "headphones") {
      $(".article").html("");
    } else {
      $(".article").html("a");
    }
    var stepLength = myRound(maxes[item]/(nBins-1), roundToNearest[item]);

    var firstColWidth = 150;
    var otherColWidth = 100;

    var lowers = [];
    var uppers = [];

    var sliderCells = ""
    var priceCells = ""
    for (var i=0; i<10; i++) {
      sliderCells += ('<td rowspan="5" width="' + otherColWidth + '" align="center"><div class="slider" id="' + sliderLabel[i] + '"></div></td>');
      if (i<(nBins-1)) {
        var lowPrice = i*stepLength;
        var highPrice = (i+1)*stepLength;
        lowers.push(lowPrice);
        uppers.push(highPrice);
        priceCells += ('<td align="center" width="' + otherColWidth + '">$' + lowPrice + '-$' + highPrice + '</td>');
      } else {
        lowers.push(maxes[item]);
        uppers.push("infty");
        priceCells += ('<td align="center" width="' + otherColWidth + '">more than $' + maxes[item] + '</td>');
      }
    }
    $("#sliders").html('<td height="80" width="' + firstColWidth + '">Extremely Likely</td>' + sliderCells);
    $("#prices").html('<td width="' + firstColWidth + '"></td>' + priceCells);


    var sliderCells = ""
    var priceCells = ""
    for (var i=10; i<nBins; i++) {
      sliderCells += ('<td rowspan="5" width="' + otherColWidth + '" align="center"><div class="slider" id="' + sliderLabel[i] + '"></div></td>');
      if (i<(nBins-1)) {
        var lowPrice = i*stepLength;
        var highPrice = (i+1)*stepLength;
        lowers.push(lowPrice);
        uppers.push(highPrice);
        priceCells += ('<td align="center" width="' + otherColWidth + '">$' + lowPrice + '-$' + highPrice + '</td>');
      } else {
        lowers.push(maxes[item]);
        uppers.push("infty");
        priceCells += ('<td align="center" width="' + otherColWidth + '">more than $' + maxes[item] + '</td>');
      }
    }
    $("#moreSliders").html('<td height="80" width="' + firstColWidth + '">Extremely Likely</td>' + sliderCells);
    $("#morePrices").html('<td width="' + firstColWidth + '"></td>' + priceCells);

    var trialData = {buyer:buyer,
                     item:item,
                     stepLength:stepLength,
                     max:maxes[item],
                     lowers:lowers,
                     uppers:uppers,
                     responses:[]};
    var nResponses = 0;

    function changeCreator(i) {
      return function(value) {
        $('#' + sliderLabel[i]).css({"background":"#99D6EB"});
        $('#' + sliderLabel[i] + ' .ui-slider-handle').css({
          "background":"#667D94",
          "border-color": "#001F29" });
        if (trialData.responses[i] == null) {
          nResponses++;
          $('.bar').css('width', ( ((qNumber*nBins + nResponses) / nClicks)*100 + "%"));
        }
        trialData.responses[i.toString()] = $("#"+sliderLabel[i]).slider("value");
        console.log(trialData.responses)
      } 
    }

    function slideCreator(i) {
      return function() {
        $('#' + sliderLabel[i] + ' .ui-slider-handle').css({
           "background":"#E0F5FF",
           "border-color": "#001F29"
        });
      }
    }

    for (var i=0; i<nBins; i++) {
      $("#" + sliderLabel[i]).attr({"width":"12px", "height":"360px", "position":"relative", "margin":"5px"});
      $("#" + sliderLabel[i] + " .ui-slider-handle").attr({"background": "#FAFAFA"});
      $('#' + sliderLabel[i]).slider({
        animate: true,
        orientation: "vertical",
        max: 1 , min: 0, step: 0.01, value: 0.5,
        slide: slideCreator(i),
        change: changeCreator(i)
      });
    }

    $("#continue").click(function() {
      if (nResponses < nBins) {
        $("#targetError").show();
      } else {
        $("#continue").unbind("click");
        $("#targetError").hide();
        experiment.data[qNumber.toString()] = trialData;
        if (qNumber + 1 < nProbQs) {
          experiment.trial(qNumber+1);
        } else {
          experiment.maxTrial(qNumber+1);
        }
      }
    })
  },

  maxTrial: function(qNumber) {
    $('.bar').css('width', ( ((nProbQs*nBins + (qNumber-nProbQs)) / nClicks)*100 + "%"));
    showSlide("maxTrial");
    $("#max").val("");
    $("#maxError").hide();

    var item = items[qNumber - nProbQs];
    $(".item").html(item);
    console.log(plural[item]);
    $(".pluralItem").html(plural[item]);

    $("#maxContinue").click(function() {
		  var response = $("#max").val();
		  var isPrice = /^[0-9]*(\.[0-9])?[0-9]$/.test(response);
		  if (isPrice) {
        $("#maxContinue").unbind("click");
        $("#targetError").hide();
        experiment.data[qNumber.toString()] = {response:response, item:item};
        if (qNumber + 1 < nQs) {
          experiment.maxTrial(qNumber+1);
        } else {
          experiment.questionaire();
        }
		  } else {
		    $("#maxError").show();
		  }
    })
  }
};
