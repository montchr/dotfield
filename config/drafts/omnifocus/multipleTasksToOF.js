/* global CallbackURL, context, draft */

const lines = draft.content.split('\n');
const baseURL = 'omnifocus://x-callback-url/add';

lines.forEach((line) => {
  if (line.length === 0) {
    return;
  }

  const cb = CallbackURL.create();
  cb.baseURL = baseURL;
  cb.addParameter('name', line);

  const success = cb.open();
  if (!success) {
    if (cb.status === 'cancel') {
      context.cancel();
    } else {
      context.fail();
    }
  }
});
