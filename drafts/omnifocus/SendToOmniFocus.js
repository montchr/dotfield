/* global CallbackURL, context */
/* eslint-disable no-console, no-unused-vars */

function SendToOmniFocus(content, options = {}) {
  const lines = content.split('\n');

  function doCallback(cb, successMessage) {
    const success = cb.open();
    if (success) {
      if (successMessage) console.log(successMessage);
    } else {
      console.log(cb.status);
      if (cb.status === 'cancel') {
        context.cancel();
      } else {
        context.fail();
      }
    }
  }

  const defaults = {
    project: '',
    tags: [],
    successMessage: 'Added to “{{project}}” in OF',
    edit: false,
  };
  const actualOptions = Object.assign({}, defaults, options);
  const { project, tags, edit } = actualOptions;
  let { successMessage } = actualOptions;

  // If the default success message hasn't been overridden,
  // replace its template tag with the project name.
  if (project && ! options.successMessage) {
    successMessage = successMessage.replace('{{project}}', project);
  }

  if (edit) {
    lines.forEach((task) => {
      const cb = CallbackURL.create();
      cb.baseURL = 'omnifocus:///add';
      cb.addParameter('name', task);
      cb.addParameter('project', project);
      tags.forEach(tag => cb.addParameter('tag', tag));
      doCallback(cb, successMessage);
    });
  } else {
    const cb = CallbackURL.create();
    const target = `/task/${encodeURI(project)}`;

    const taskpaperLines = lines.map((line) => {
      let lineContent = line;
      lineContent = `- ${line}`;
      if (tags.length > 0) {
        lineContent += ` @tags(${tags.toString()})`;
      }
      return lineContent;
    });
    const taskpaper = taskpaperLines.join('\n');

    cb.baseURL = 'omnifocus:///paste';
    cb.addParameter('target', target);
    cb.addParameter('content', taskpaper);
    doCallback(cb, successMessage);
  }
}
