var typeDelay = function() {
  return 60 + 20 * Math.random();
};

var typeString = function(string) {
  var commands = [];
  for (var i = 0; i < string.length; i++) {
    var char = string[i];
    if (char != ' ' && Math.random() < 0.01) {
      commands.push({
        type: 'type',
        text: String.fromCharCode(char.charCodeAt(0) + 1),
        delay: typeDelay() * 2
      });
      commands.push({
        type: 'backspace',
        delay: typeDelay() * 2
      });
    };
    commands.push({
      type: 'type',
      text: char,
      delay: typeDelay()
    });
  }
  return commands;
};

var eraseString = function(string) {
  var commands = [];
  for (var i = 0; i < string.length; i++) {
    commands.push({
      type: 'backspace',
      delay: i == 0 ? 400 : 40
    });
  }
  return commands;
};

var forEachDelay = function(list, fn) {
  var i = 0;
  var executeNext;

  var schedule = function(timeout) {
    setTimeout(function() {
      executeNext();
    }, timeout);
  };

  executeNext = function() {
    var timeout = fn(script[i]);
    i = (i + 1) % list.length;
    schedule(timeout);
  };

  executeNext();
};

var reminders = [
  ["Tuesday at 4:00 PM", "Please get another haircut"],
  ["Sun 1am", "Go to bed. You have work in the morning."],
  ["Sept 3 9:00am", "Pick up dolphin from camp"],
  ["tomorrow at 3pm", "Fall in love all over again"],
  ["5:00 friday", "stop lying to yourself about your tree house"],
  ["three days from now", "check if the avocados are ripe yet"],
  ["6th of October", "marvin and the hamsters is out on dvd!"],
  ["eventually", "Tell Susie how you feel"]
].sort(function() {
  return Math.random() < 0.5 ? -1 : 1;
});

var subject = function(entry) {
  return entry[0];
};

var body = function(entry) {
  return entry[1];
};

var scriptForReminder = function(lastEntry, entry) {
  return eraseString(subject(lastEntry))
    .concat([{ type: 'noop', delay: 200 }])
    .concat(typeString(subject(entry)))
    .concat([{ type: 'noop', delay: 200 }])
    .concat([{ type: 'tab', delay: 500 }])
    .concat(eraseString(body(lastEntry)))
    .concat([{ type: 'noop', delay: 200 }])
    .concat(typeString(body(entry)))
    .concat([{ type: 'noop', delay: 2000 }])
    .concat([{ type: 'tab', delay: 500 }]);
};

var getReminder = function(i) {
  return reminders[(i + reminders.length) % reminders.length];
}

var script = [{ type: 'noop', delay: 2000 }];
for (var i = 1; i <= reminders.length; i++) {
  script = script.concat(scriptForReminder(getReminder(i - 1), getReminder(i)));
}

document.addEventListener('DOMContentLoaded', function() {
  var bodyEl = document.querySelector('.window-content > .body');
  var subjectEl = document.querySelector('.window-content > .header > :nth-child(3) > .content');
  var cursorEl = document.querySelector('.cursor');

  var els = [subjectEl, bodyEl];
  var texts = reminders[0];
  var elIndex = 0;

  forEachDelay(script, function(command) {
    console.log(command);
    switch (command.type) {
      case "type":
        texts[elIndex] += command.text;
        break;
      case "backspace":
        texts[elIndex] = texts[elIndex].substr(0, texts[elIndex].length - 1);
        break;
      case "noop":
        break;
      case "tab":
        elIndex = (elIndex + 1) % els.length;
        break;
    }
    for (var i = 0; i < els.length; i++) {
      els[i].innerText = texts[i];
    }
    els[elIndex].appendChild(cursorEl);
    return command.delay;
  });
});
