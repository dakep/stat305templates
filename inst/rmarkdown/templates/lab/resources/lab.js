/* global: Shiny, $ */
window.lab = (function () {
'use strict'

const MAX_TIMEOUT = 1000
const SHINYJS_LOAD_TIMEOUT = 250
const spin = $('<div class="spin-container"><div class="spinner"></div></div>')

var labId = 'unknown_lab'
var openConfirmationDialog = null


function triggerMathJax () {
  window.MathJax && MathJax.Hub.Queue(["Typeset", MathJax.Hub])
}

function loadShinyJs () {
  try {
    Shiny.addCustomMessageHandler('shinyjs-inject', function(content) { $("head").append(content) })
  } catch (e) {
    // If it's not working, try again after a while!
    window.setInterval(loadShinyJs, SHINYJS_LOAD_TIMEOUT)
  }
}

function checkLabNameInput() {
  const invalidInputs = $(this).filter(function () {
    return ($(this).val().length < 3)
  })

  if (invalidInputs.length > 0) {
    $('.submit-lab').prop('disabled', true).addClass('disabled')
  } else {
    $('.submit-lab').prop('disabled', false).removeClass('disabled')
  }
}

/* Code to run beforehand */
$(document).ready(function () {
  $('body').append(spin)

  // Load shinyjs
  loadShinyJs()

  $('.lab-student-name input')
    .on('change', checkLabNameInput)
    .each(checkLabNameInput)


  // Show the spinner until shiny is idling the first time.
  spin.show()
  $(document).one('shiny:idle', function () {
    spin.hide()
  })

  // Trigger MathJax for changed inputs
  $('.shiny-input-radiogroup,.shiny-input-checkboxgroup').on('shiny:inputchanged', triggerMathJax)

  if (window.labId) {
    labId = window.labId
    delete window.labId
    window.shinyClientStore.initialize(labId)
  }
})

return {
  setLabId: function (_labId) {
    labId = _labId
    window.shinyClientStore.initialize(labId)
  },
  getLabId: function (_labId) {
    return labId
  },

  triggerMathJax: triggerMathJax,

  showDialog: function (idPrefix, success) {
    if (success) {
      const dialog = $('#' + idPrefix + 'success')
      dialog.appendTo('body')
      dialog.modal('show')
    } else {
      showErrorDialog($('#' + idPrefix + 'error'))
    }
    if (openConfirmationDialog) {
      openConfirmationDialog.modal('hide')
      openConfirmationDialog = null
    }
  }
}
}())
