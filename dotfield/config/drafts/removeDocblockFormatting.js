/* global draft */

/**
 * Cleans up lines beginning with spaces+asterisks as a result of
 * JSDoc/PHPDoc/Javadoc/etc.
 *
 * @author Chris Montgomery <chris@montchr.io>
 */

const lines = draft.content.split('\n');
const pattern = /^\s+\*\s/;
const cleanLines = lines.map((line) => line.replace(pattern, ''));
draft.content = cleanLines.join('\n');
