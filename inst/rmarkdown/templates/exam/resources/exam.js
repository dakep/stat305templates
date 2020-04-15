/* global: Shiny, $ */
window.exam = (function () {
'use strict'

const MAX_TIMEOUT = 1000
const spin = $('<div class="spin-container"><div class="spinner"></div></div>')


var examId = 'unknown_exam'
var openConfirmationDialog = null

function collectAnswers () {
  const aceEditorValues = $('.tutorial-exercise-code-editor.ace_editor').map(function () {
    const value = ace.edit(this).getValue().trim()
    if (value) {
      return {type: "ace", name: $(this).attr('id'), value: ace.edit(this).getValue().trim()}
    } else {
      return null
    }
  }).get()
  const radioValues = $('input[type="radio"]').map(function () {
    const input = $(this)
    if (input.prop('checked')) {
      return {type: "radio", name: input.attr('name'), value: input.val()}
    } else {
      return null
    }
  }).get()

  return aceEditorValues.concat(radioValues)
}


function showErrorDialog (dialog) {
  dialog.find('button[data-exam="reload"').click(function () {
    window.location.reload()
  })
  var currentAnswerOutput = dialog.find('.code')
  if (currentAnswerOutput.length < 1) {
    currentAnswerOutput = $('<div class="code"></div>')
    dialog.find('div.modal-body').append(currentAnswerOutput)
  }
  currentAnswerOutput.text(btoa(JSON.stringify(collectAnswers())))

  if (openConfirmationDialog) {
    openConfirmationDialog.modal('hide')
    openConfirmationDialog = null
  }

  dialog.appendTo('body')
  dialog.modal('show')
  $('#shiny-disconnected-overlay').hide()
}

function checkSpin(errorDialog) {
  // If the spinner is still visible, the server did not respond in time!
  if (spin.is(':visible')) {
    showErrorDialog(errorDialog)
    spin.hide()
  }
}

function submitSection(event, extraParameters) {
  const submitButton = $(this)
  const confirmDialog = $('#' + submitButton.data('confirmdialogid'))

  if (!extraParameters) {
    extraParameters = {}
  }

  // Run all visible code chunks, unless disabled by extra parameters
  if (!extraParameters.dontRunChunks) {
    // Get all visible and non-empty chunks
    const visibleChunks = $('.tutorial-exercise:visible').filter(function () {
      try {
        return ace.edit($(this).find(".ace_editor").get(0)).getValue().trim().length > 0
      } catch (e) {
        window.console.warn("Can not determine if editor is empty. Assuming it is empty!", e)
      }
      return false
    })
    var chunksLeft = visibleChunks.length

    if (chunksLeft > 0) {
      event.stopImmediatePropagation()
      spin.show()
      visibleChunks.each(function () {
        $(this).one('shiny:value', function () {
          chunksLeft -= 1
          if (chunksLeft <= 0) {
            extraParameters.dontRunChunks = true
            submitButton.trigger('click', extraParameters)
          }
        }).find('.btn-tutorial-run').trigger('click')
      })

      return false
    }
  }

  spin.hide()

  // All code chunks have run. Show confirmation dialog or submit directly
  if (confirmDialog.length > 0 && !extraParameters.passThrough) {
    event.stopImmediatePropagation()

    // Show modal
    openConfirmationDialog = confirmDialog
    confirmDialog.modal('show')

    confirmDialog.find('button[data-exam="continue"]').click(function () {
      spin.show()
      extraParameters.passThrough = true
      submitButton.trigger('click', extraParameters)
      confirmDialog.modal('hide')
      window.setTimeout(checkSpin, MAX_TIMEOUT, $('#' + submitButton.data('errordialogid')))
    })
    return false
  }
}

function storeAnswers () {
  const currentValues = collectAnswers()
  window.localStorage.setItem(examId + '_answers', JSON.stringify(currentValues))
}

function setRadioFromStore (name) {
  try {
    const storedValues = JSON.parse(window.localStorage.getItem(examId + '_answers'))
    if (storedValues) {
      storedValues.forEach(function (input) {
        if (input.name == name) {
          $('input[name="' + input.name + '"]').val([input.value])
        }
      })
    }
  } catch (e) {
    window.console.warn(e)
  }
}

function triggerMathJax() {
  window.MathJax && MathJax.Hub.Queue(["Typeset", MathJax.Hub])
}

/* Code to run beforehand */
$(document).ready(function () {
  $('body').append(spin)
  // Interrupt submission of sections
  $('button.submit-section').click(submitSection)

  // Show the spinner until shiny is idling the first time.
  spin.show()
  $(document).one('shiny:idle', function () {
    spin.hide()
  })

  // Ensure that every radio button is saved!
  $('body').on('change', 'input[type="radio"]', function (event) {
    storeAnswers()
  })

  // Once the input is loaded
  $('.shiny-input-radiogroup').one('shiny:inputchanged', function (event) {
    setRadioFromStore(event.name)
  })

  // Ensure mathjax is run after input changes
  $('.shiny-input-radiogroup').on('shiny:inputchanged', triggerMathJax)

  // Load shinyjs
  Shiny.addCustomMessageHandler('shinyjs-inject', function(content) {$("head").append(content)})
})

return {
  setExamId: function (_examId) {
    examId = _examId
    $('.shiny-input-radiogroup input[type="radio"]').each(function() {
      setRadioFromStore($(this).attr('name'))
    })
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
