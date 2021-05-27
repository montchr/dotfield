/* global cancel, editor, SendToOmniFocus, Prompt */

/**
 * Adds the Draft as TaskPaper content to OmniFocus
 */

const content = editor.getText();

const prompt = Prompt.create();
prompt.addButton('Music');
prompt.addButton('Video');
prompt.addButton('General');
prompt.addButton('Reading');
prompt.addButton('Restaurants');
prompt.addSwitch('edit', 'Edit in OmniFocus?', false);
const promptSubmitted = prompt.show();
if (promptSubmitted === false) {
  cancel('User cancelled the script');
}
const { buttonPressed } = prompt;
// Edit the task before adding to OmniFocus?
const { edit } = prompt.fieldValues;

let projectName = '';
switch (buttonPressed) {
  case 'Music':
    projectName = 'Music Collection';
    break;
  case 'General':
    projectName = 'General Investigation';
    break;
  default:
    projectName = buttonPressed;
    break;
}

SendToOmniFocus(content, {
  project: `Investigate : ${projectName}`,
  successMessage: 'Taskpaper added to OF',
  edit,
});
