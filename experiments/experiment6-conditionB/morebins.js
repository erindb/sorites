function caps(a) {return a.substring(0,1).toUpperCase() + a.substring(1,a.length);}
function uniform(a, b) { return ( (Math.random()*(b-a))+a ); }
function showSlide(id) { $(".slide").hide(); $("#"+id).show(); }
function shuffle(v) { newarray = v.slice(0);for(var j, x, i = newarray.length; i; j = parseInt(Math.random() * i), x = newarray[--i], newarray[i] = newarray[j], newarray[j] = x);return newarray;} // non-destructive.

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

pronoun = {"watch":"It was",
           "laptop":"It was",
           "coffee maker":"It was",
           "headphones":"They were",
           "sweater":"It was"};

stepLength = {
  "watch":50,
  "laptop":50,
  "coffee maker":4,
  "headphones":6,
  "sweater":3
}
maximum = {
  "watch":3000,
  "laptop":2500,
  "coffee maker":270,
  "headphones":330,
  "sweater":240
}
function nBins(item) {
  return Math.ceil(maximum[item] / stepLength[item]);
}

var plural = {"watch":"watches",
              "laptop":"laptops",
              "coffee maker":"coffee makers",
              "headphones":"headphones",
              "sweater":"sweaters"}

var nQs = items.length;
var nClicks = 0;
for (var i=0; i<nQs; i++) {
  nClicks += nBins(items[i]);
}
console.log(nClicks);
var nComplete = 0;

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
    $('.bar').css('width', ( (nComplete / nClicks)*100 + "%"));
    showSlide("trial");

    var item = items[qNumber];
    var buyer = buyers[qNumber];

    var nRows = Math.ceil(nBins(item) / 10);
    var allthatjazz = '';
    for (var i=0; i<nRows; i++) {
      allthatjazz += '<p id="statement"><span class="buyer">{{}}</span> bought ' +
                      ' <span class="article">{{}}</span> new <i><b><span class="item">{{}}</span></b></i>. ' +
                      pronoun[item] + " <b><i>expensive</i></b>." +
                      '</p><p id="question">Please rate how likely it is that the cost of the ' +
                      '<span class="item">{{}}</span> is within each of the following ranges.</p>' +
                      '<table><tbody><tr id="sliders' + i +
                     '"></tr><tr><td height="72">Very Likely</td></tr>' +
                     '<tr><td height="72">Neutral</td></tr>' +
                     '<tr><td height="72">Not Very Likely</td></tr>' +
                     '<tr><td height="72">Extremely Unlikely</td></tr>' +
                     '<tr id="prices' + i + '"></tr></tbody></table><hr/>'
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
        priceCells += '<td align="center" width="' + otherColWidth + '">$' + lowPrice + '-$' + highPrice + '</td>';
      } else {
        var lowPrice = i*stepLength[item];
        lowers.push(lowPrice);
        uppers.push("infty");
        priceCells += '<td align="center" width="' + otherColWidth + '">more than $' + lowPrice + '</td>';
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
                     max:maximum[item],
                     lowers:lowers,
                     uppers:uppers,
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
