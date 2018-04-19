$(document).keydown(function(event) {
  if ($("#pm_store_name").is(":focus") && event.keyCode == 13) {
    $("#pm_store").click();
  } else if ($("#ff_store_name").is(":focus") && event.keyCode == 13) {
    $("#ff_store").click();
  } else if ($("#km_store_name").is(":focus") && event.keyCode == 13) {
    $("#km_store").click();
  } else if ($("#ca_store_pred_name").is(":focus") && event.keyCode == 13) {
    $("#ca_store_pred").click();
  } else if ($("#ca_store_pw_name").is(":focus") && event.keyCode == 13) {
    $("#ca_store_pw").click();
  } else if ($("#ca_store_iw_name").is(":focus") && event.keyCode == 13) {
    $("#ca_store_iw").click();
  }
});
