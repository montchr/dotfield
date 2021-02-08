/* global cancel, editor, SendToOmniFocus, Prompt */

const MY_STORES = [
  'amazon',
  'cousins',
  'fine fare',
  'hardware',
  'liberty choice',
  'pharmacy',
  'spring garden market',
  'whole foods',
];

const content = editor.getText();
let tags = [];

const prompt = Prompt.create();
prompt.addButton('Groceries');
prompt.addButton('General Shopping');
prompt.addSwitch('edit', 'Edit in OmniFocus?', false);
const promptSubmitted = prompt.show();
if (promptSubmitted === false) {
  cancel('User cancelled the script');
}

// Edit the task before adding to OmniFocus?
const { edit } = prompt.fieldValues;

if (prompt.buttonPressed === 'Groceries') {
  tags.push('groceries');
}

// Prompt the user to select stores and add the stores to tags
const storesPrompt = Prompt.create();
storesPrompt.addSelect('stores', 'Choose Stores:', MY_STORES, [], true);
storesPrompt.addButton('Submit');
const storesPromptSubmitted = storesPrompt.show();
if (storesPromptSubmitted === false) {
  cancel('User cancelled the script');
}
const selectedStores = storesPrompt.fieldValues.stores;
if (selectedStores.length > 0) {
  tags = tags.concat(selectedStores);
}

SendToOmniFocus(content, {
  project: 'Shopping',
  tags,
  edit,
});
