/* global draft */

/**
 * Tags to Hashtags
 *
 * Converts Drafts tags to hashtags and appends them to the content if the
 * hashtags aren’t already present.
 *
 * Has the same single-word hashtag limitation as similar scripts. Any Drafts
 * tags containing spaces will be converted to a hashtag with no spaces. Why
 * would you want a hashtag with spaces anyway?
 *
 * Basically does the opposite of @derekvan's "Tag drafts by hashtag" <http://actions.getdrafts.com/a/1Q7>
 *
 * Based on @kjaymiller's "Add Hashtags from Inside of Draft" action <http://actions.getdrafts.com/a/1ME>
 *
 * @author Chris Montgomery <chris@montchr.io>
 * @link https://actions.getdrafts.com/a/1Uu
 */

const { content, tags } = draft;

if (tags && tags.length !== 0) {
  const spacelessTags = tags.map(tag => tag.replace(' ', ''));
  let newTags = spacelessTags;

  const re = /#[\w\d]+/g;
  const hashtags = content.match(re);

  // Get the draft tags that don't already have hashtag equivalents in the content
  if (hashtags) {
    const hashlessHashtags = hashtags.map(tag => tag.replace('#', ''));
    newTags = spacelessTags.filter(tag => !hashlessHashtags.includes(tag));
  }

  // Append the new tags to the content
  if (newTags.length > 0) {
    const newHashtags = newTags.map(tag => `#${tag}`);
    draft.content += `\n\n${newHashtags.join(' ')}`;
  }
}
