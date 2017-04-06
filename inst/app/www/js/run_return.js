// based on http://stackoverflow.com/a/32340906/1974918
// and http://stackoverflow.com/a/8774101/1974918
$(document).keydown(function(event) {
  if ($("#pf_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#pf_run").click();
  } else if ($("#ff_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#ff_run").click();
  } else if ($("#hc_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#hc_run").click();
  } else if ($("#km_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#km_run").click();
  } else if ($("#mds_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#mds_run").click();
  } else if ($("#pm_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#pm_run").click();
  } else if ($("#ca_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#ca_run").click();
  }
});
