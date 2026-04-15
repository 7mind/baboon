import { readFileSync, readdirSync, statSync } from 'fs';
import { join } from 'path';
import { buildSchema, parse } from 'graphql';

function findGraphqlFiles(dir) {
  const results = [];
  for (const entry of readdirSync(dir)) {
    const full = join(dir, entry);
    if (statSync(full).isDirectory()) {
      results.push(...findGraphqlFiles(full));
    } else if (entry.endsWith('.graphql')) {
      results.push(full);
    }
  }
  return results;
}

const schemasDir = process.argv[2];
if (!schemasDir) {
  console.error('Usage: node validate.mjs <schemas-directory>');
  process.exit(1);
}

const files = findGraphqlFiles(schemasDir);
if (files.length === 0) {
  console.error(`No .graphql files found in ${schemasDir}`);
  process.exit(1);
}

let errors = 0;

for (const file of files.sort()) {
  const content = readFileSync(file, 'utf-8');

  // Step 1: Parse the SDL (checks syntax)
  try {
    parse(content);
  } catch (e) {
    console.error(`PARSE ERROR in ${file}: ${e.message}`);
    errors++;
    continue;
  }

  // Step 2: Build schema (checks type references, consistency)
  // We add a dummy Query type since buildSchema requires it,
  // but only if the schema doesn't already define one.
  const schemaWithQuery = content.includes('type Query') ? content : content + '\ntype Query { _unused: Boolean }\n';
  try {
    buildSchema(schemaWithQuery);
    console.log(`OK: ${file}`);
  } catch (e) {
    console.error(`SCHEMA ERROR in ${file}: ${e.message}`);
    errors++;
  }
}

console.log(`\nValidated ${files.length} files, ${errors} errors.`);
if (errors > 0) {
  process.exit(1);
}
