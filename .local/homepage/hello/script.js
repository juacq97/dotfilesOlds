function setDate() {
  let date = $("#date");
  date.text("Today is ");
  date.append(moment().format("dddd") + ", ");
  date.append(moment().format("Do") + " of ");
  date.append(moment().format("MMMM"));
}

function setWeather() {
  $("#weather-temp").openWeather({
    key: "_key_",
    city: "_city_",
    descriptionTarget: "#weather-desc",
    success: function() {
      $("#weather").show();
      showContent();
    },
    error: showContent
  });
}

function showContent() {
  $("#content").fadeIn().css("display","inline-block");
}

$(document).ready(function() {
  setDate();
  setWeather();
});
