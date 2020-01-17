/* global: Cookies, Shiny, jQuery */
(function () {
'use strict';

function renderClickerQuestions() {
  $('.section.level3').each(function() {
    const el = jQuery(this);

    if (!el.hasClass('no-clicker')) {
      el.addClass('clicker');

      if (!el.hasClass('no-collapse')) {
        el.addClass('collapse');
        const button = $('<a class="btn btn-primary show-clicker" role="button" data-toggle="collapse" href="#" aria-expanded="false" aria-controls="collapseClickerQuestion">Show Clicker Question…</a>');
        button.attr('href', '#' + el.attr('id'));
        button.insertBefore(el);
      }
    }

  });
}

$(document).ready(function() {
  renderClickerQuestions();
});

$(document).keydown(function (event) {
  if (event.key == 'ArrowLeft') {
    $('.topicActions button.btn-default').eq(0).click();
  } else if (event.key == 'ArrowRight') {
    $('.topicActions button.btn-primary').eq(0).click();
  }
});

}());