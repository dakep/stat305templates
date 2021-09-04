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
  const validationContainer = $(this).find('div.alert')
  const nameInput = $(this).find('input[id$="student-name"]').val()
  const idInput = $(this).find('input[id$="student-nr"]').val()
  const errorMessages = validationContainer.data('messages')

  var errorMessage = ""
  if (errorMessages.studentNameEmpty && nameInput.length < 3) {
    errorMessage = errorMessages.studentNameEmpty
  } else if (errorMessages.studentIdWrongLength && idInput.length != 8) {
    errorMessage = errorMessages.studentIdWrongLength
    errorMessage = "The " + labels.studentId.toLowerCase() + " must be exactly 8 digits."
  } else if (errorMessages.studentIdWrongFormat && !/\d{8}/.test(idInput)) {
    errorMessage = errorMessages.studentIdWrongFormat
    errorMessage = "The " + labels.studentId.toLowerCase() + " must contain only numbers."
  }

  validationContainer.text(errorMessage)
  if (errorMessage.length > 0) {
    validationContainer.removeClass('hidden')
    $('.submit-lab').prop('disabled', true).addClass('disabled')
  } else {
    validationContainer.addClass('hidden')
    $('.submit-lab').prop('disabled', false).removeClass('disabled')
  }
}

/* Code to run beforehand */
$(document).ready(function () {
  $('body').append(spin)

  // Load shinyjs
  loadShinyJs()

  $('.lab-student-name')
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
