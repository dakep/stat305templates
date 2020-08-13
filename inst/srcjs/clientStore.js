/* global: Shiny, $ */
/**
 * Returns a function, that, as long as it continues to be invoked, will not
 * be triggered. The function will be called after it stops being called for
 * N milliseconds. If `immediate` is passed, trigger the function on the
 * leading edge, instead of the trailing. The function also has a property 'clear'
 * that is a function which will clear the timer to prevent previously scheduled executions.
 *
 *  This function is licensed under the MIT license, copyright (c) 2012-2018 The Debounce Contributors.
 *  See https://github.com/component/debounce/blob/master/CONTRIBUTORS and
*   https://github.com/component/debounce/blob/master/LICENSE
 *
 * @source underscore.js
 * @see http://unscriptable.com/2009/03/20/debouncing-javascript-methods/
 * @param {Function} function to wrap
 * @param {Number} timeout in ms (`100`)
 * @param {Boolean} whether to execute at the beginning (`false`)
 * @api public
 */
function debounce(func, wait, immediate) {
  var timeout, args, context, timestamp, result;
  if (null == wait) wait = 100;

  function later() {
    var last = Date.now() - timestamp;

    if (last < wait && last >= 0) {
      timeout = setTimeout(later, wait - last);
    } else {
      timeout = null;
      if (!immediate) {
        result = func.apply(context, args);
        context = args = null;
      }
    }
  };

  var debounced = function(){
    context = this;
    args = arguments;
    timestamp = Date.now();
    var callNow = immediate && !timeout;
    if (!timeout) timeout = setTimeout(later, wait);
    if (callNow) {
      result = func.apply(context, args);
      context = args = null;
    }

    return result;
  };

  debounced.clear = function() {
    if (timeout) {
      clearTimeout(timeout);
      timeout = null;
    }
  };

  debounced.flush = function() {
    if (timeout) {
      result = func.apply(context, args);
      context = args = null;

      clearTimeout(timeout);
      timeout = null;
    }
  };

  return debounced;
};

window.shinyClientStore = (function () {
'use strict'

const LOAD_FROM_STORE_DEBOUNCE = 250

var storeId = null

function collectAnswers () {
  const aceEditorValues = $('.tutorial-exercise-code-editor.ace_editor').map(function () {
    const value = ace.edit(this).getValue().trim()
    if (value) {
      return {type: "ace", name: $(this).attr('id'), value: ace.edit(this).getValue().trim()}
    } else {
      return null
    }
  }).get()

  const inputValues = $('.shiny-input-container input').map(function () {
    const input = $(this)
    if (input.prop('checked')) {
      return {type: "radio-or-cb", name: input.attr('name'), value: input.val()}
    } else if (input.attr('type') == 'text') {
      return {type: "text", name: input.attr('id'), value: input.val()}
    } else {
      return null
    }
  }).get()

  return aceEditorValues.concat(inputValues)
}

function storeAnswers (event) {
  if (!$(this).data('simulatedEvent')) {
    const currentValues = collectAnswers()
    window.localStorage.setItem(storeId + '_answers', JSON.stringify(currentValues))
  } else {
    $(this).data('simulatedEvent', false)
  }
}

function setTextInputFromStore (id) {
  try {
    const storedValues = JSON.parse(window.localStorage.getItem(storeId + '_answers'))
    if (storedValues) {
      storedValues.forEach(function (input) {
        if (input.name == id) {
          $('#' + id).val(input.value).change()
        }
      })
    }
  } catch (e) {
    window.console.warn(e)
  }
}

function setRadioCbFromStore (name) {
  try {
    const storedValues = JSON.parse(window.localStorage.getItem(storeId + '_answers'))
    if (storedValues) {
      storedValues.forEach(function (input) {
        if (input.name == name) {
          const inputElement = $('input[name="' + name + '"]').filter('input[value="' + input.value + '"]')
          if (!inputElement.prop('checked')) {
            inputElement.data('simulatedEvent', true).trigger('click', [ true ])
          }
        }
      })
    }
  } catch (e) {
    window.console.warn(e)
  }
}

function reloadFromStore () {
  const radioCbNames = new Set()
  const textNames = new Set()
  $('.shiny-input-container input').each(function() {
    const input = $(this)
    if (input.attr('type') == 'checkbox' || input.attr('type') == 'radio') {
      radioCbNames.add(input.attr('name'))
    } else {
      textNames.add(input.attr('id'))
    }
  })
  radioCbNames.forEach(setRadioCbFromStore)
  textNames.forEach(setTextInputFromStore)
}

return {
  initialize: function (_storeId) {
    storeId = _storeId

    const debouncedReloadFromStore = debounce(reloadFromStore, LOAD_FROM_STORE_DEBOUNCE)

    $(document).one('shiny:idle', debouncedReloadFromStore)

    // Ensure that every radio button is saved!
    $('body').on('change', '.shiny-input-container input', storeAnswers)

    // The first time the input changes, set the stored answers
    $('.shiny-input-container').one('shiny:inputchanged', debouncedReloadFromStore)
  },
  getStoreId: function () {
    return storeId
  }
}
}())
