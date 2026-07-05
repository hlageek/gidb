$( document ).ready(function() {
// # ── Blur-only JS binding (text inputs AND textareas) ─────────────────────
//     # Both <input class="blur-input"> and <textarea class="blur-textarea"> only
//     # report their value to the server on blur (focus lost), not on each keystroke.

        // ── blur binding for <input> ──────────────────────────────────────────
        var blurInputBinding = new Shiny.InputBinding();
        $.extend(blurInputBinding, {
          find:      function(scope) { return $(scope).find('input.blur-input'); },
          getValue:  function(el)    { return el.value; },
          setValue:  function(el, v) { el.value = v; },
          subscribe: function(el, cb) {
            $(el).on('blur.blurInput', function() { cb(true); });
          },
          unsubscribe:    function(el)    { $(el).off('.blurInput'); },
          receiveMessage: function(el, d) { if ('value' in d) el.value = d.value; },
          getState:       function(el)    { return { value: el.value }; },
          getRatePolicy:  function()      { return { policy: 'direct' }; }
        });
        Shiny.inputBindings.register(blurInputBinding, 'shiny.blurInput');

        // ── blur binding for <textarea> ───────────────────────────────────────
        var blurTextareaBinding = new Shiny.InputBinding();
        $.extend(blurTextareaBinding, {
          find:      function(scope) { return $(scope).find('textarea.blur-textarea'); },
          getValue:  function(el)    { return el.value; },
          setValue:  function(el, v) { el.value = v; },
          subscribe: function(el, cb) {
            $(el).on('blur.blurTextarea', function() { cb(true); });
          },
          unsubscribe:    function(el)    { $(el).off('.blurTextarea'); },
          receiveMessage: function(el, d) { if ('value' in d) el.value = d.value; },
          getState:       function(el)    { return { value: el.value }; },
          getRatePolicy:  function()      { return { policy: 'direct' }; }
        });
        Shiny.inputBindings.register(blurTextareaBinding, 'shiny.blurTextarea');
      });
