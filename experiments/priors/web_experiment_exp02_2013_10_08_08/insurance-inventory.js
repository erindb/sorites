function caps(a) {return a.substring(0,1).toUpperCase() + a.substring(1,a.length);}
function uniform(a, b) { return ( (Math.random()*(b-a))+a ); }
function showSlide(id) { $(".slide").hide(); $("#"+id).show(); }
function shuffle(v) { var newarray = v.slice(0); for(var j, x, i = newarray.length; i; j = parseInt(Math.random() * i), x = newarray[--i], newarray[i] = newarray[j], newarray[j] = x);return newarray;} // non-destructive.
function sample(v) { var shuffled = shuffle(v); return shuffled[0];}
function rndInt() { var i = Math.floor(Math.random()*10); if (i==1) {return 9;} else {return i;}}
function rndAlpha() { return sample("abcdefghijklmnopqrstuvwxyz".split("")); }
function bernouli() { var n = Math.random(); return (n > 0.5);}
function rndAlphaNum() {if (bernouli()) {return rndAlpha();} else {return rndInt();}}

var nQs = 1;
var cutoffs = [50, 100, 500, 1000, 2000, 5000, 10000];
var cutoff = sample(cutoffs);
var items = ["watch", "coffee maker", "laptop", "electric kettle", "headphones", "sweater", "camera", "backpack", "wallet", "bike"];
var order = []
for (var i=0; i<items.length; i++) {
  var item = items[i];
  order.push({item:item, bin:"above"});
  order.push({item:item, bin:"below"});
}
var order = shuffle(order);

$(document).ready(function() {
  showSlide("consent");
  $("#mustaccept").hide();
  $(".cutoff").html("$"+cutoff);
});

var experiment = {
  data: {cutoff:cutoff},
  
  instructions: function() {
    if (turk.previewMode) {
      $("#instructions #mustaccept").show();
    } else {
      showSlide("instructions");
      $("#begin").click(function() { experiment.trial(0); })
    }
  },
  
  trial: function(qNumber) {
    $('.bar').css('width', ( (qNumber / nQs)*100 + "%"));
    $(".error").hide();
    showSlide("trial");

    var table = "<table border=1 style='text-align:center' cellpadding=5><tr><td>Item Number</td><td>Description</td><td>Price</td>";
    for (var i=0; i<order.length; i++){
      itemNum = ""
      for (var j=0; j<4; j++) {
        itemNum += rndAlphaNum();
      }
      var item = order[i].item;
      var bin = order[i].bin;
      var style;
      if (bin == "above") {
        style = "style='color:#0000ff;font-style:italic'";
      } else if (bin == "below") {
        style = "";
      } else {
        console.log("error 1");
      }
      table += "<tr " + style + "><td>" + itemNum + "</td><td>" + item + "</td><td>$<input " + style + " type='text' name='" + item + "-" + bin + "'></input></td>";
    }
    table += "</table";
    $("#inventory").html(table)
    $("#continue").click(function() {
      $(".error").hide();
      var rawResponse = $("#trialform").serialize();
      var pieces = rawResponse.split("&");
      for (var i=0; i<pieces.length; i++) {
        var bin = order[i].bin;
        var response = pieces[i].split("=")
        if (/^[0-9]*(\.[0-9])?[0-9]$/.test(response[1])) {
          var price = parseFloat(response[1]);
          var rightBin;
          if (bin == "above") {
            rightBin = price > cutoff;
          } else if (bin == "below") {
            rightBin = price <= cutoff;
          } else {
            console.log("error 2");
          }
          if (rightBin) {
            experiment.data[response[0]] = price;
          } else {
            $("#wrongBin").show();
            return false;
          }
        } else {
          $("#invalidPrice").show();
          return false;
        }
      }
      if (qNumber + 1 < nQs) {
        experiment.trial(qNumber+1);
      } else {
        experiment.questionaire();
      }
    })
  },
  
  questionaire: function() {
    //disable return key
    $(document).keypress( function(event){
     if (event.which == '13') {
        event.preventDefault();
      }
    });
    showSlide("questionaire");
    $("#formsubmit").click(function() {
      var rawResponse = $("#questionaireform").serialize();
      var pieces = rawResponse.split("&");
      var age = pieces[0].split("=")[1];
      var lang = pieces[1].split("=")[1];
      var comments = pieces[2].split("=")[1];
      if (lang.length > 0) {
        experiment.data["language"] = lang;
        experiment.data["comments"] = comments;
        experiment.data["age"] = age;
        showSlide("finished");
        setTimeout(function() { turk.submit(experiment.data) }, 1000);
      }
    });
  }
}
  
