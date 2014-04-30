Shiny.addCustomMessageHandler("replace_textarea",
  function(message) {
    if (typeof message === 'object') {
      // an error must have occurred
      var msg = '<pre class="alert alert-error" style="word-wrap: normal; white-space: pre">'
                + message[0] + '</pre>';
      $('textarea#src').popover('destroy')
                       .popover({
                         placement: 'bottom',
                         html: true,
                         title: 'Failed to format the code',
                         content: msg
                       })
                       .popover('show')
                       .addClass('alert-error');
    } else {
      $('textarea#src').popover('destroy').removeClass('alert-error').val(message);
    };
  }
);
