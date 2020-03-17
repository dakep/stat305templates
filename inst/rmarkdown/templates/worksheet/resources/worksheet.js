/* global: Cookies, Shiny, jQuery */
(function () {
'use strict';

$(document).keydown(function (event) {
  if (event.key == 'ArrowLeft') {
    $('.topicActions button.btn-default').eq(0).click();
  } else if (event.key == 'ArrowRight') {
    $('.topicActions button.btn-primary').eq(0).click();
  }
});

}());
