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

$(document).keydown(function(event) {
  if ($("#conjoint_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#conjoint_report").click();
  } else if ($("#full_factor_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#full_factor_report").click();
  } else if ($("#hclus_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#hclus_report").click();
  } else if ($("#kclus_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#kclus_report").click();
  } else if ($("#mds_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#mds_report").click();
  } else if ($("#pmap_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#pmap_report").click();
  } else if ($("#pre_factor_report").is(":visible") && event.altKey && event.keyCode == 13) {
    $("#pre_factor_report").click();
  }
});
