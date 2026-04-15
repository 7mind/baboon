import { readFileSync, readdirSync, statSync } from 'fs';
import { join } from 'path';
import SwaggerParser from '@apidevtools/swagger-parser';

function findJsonFiles(dir) {
  const results = [];
  for (const entry of readdirSync(dir)) {
    const full = join(dir, entry);
    if (statSync(full).isDirectory()) {
      results.push(...findJsonFiles(full));
    } else if (entry.endsWith('.json')) {
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

const files = findJsonFiles(schemasDir);
if (files.length === 0) {
  console.error(`No .json files found in ${schemasDir}`);
  process.exit(1);
}

let errors = 0;

for (const file of files.sort()) {
  const content = readFileSync(file, 'utf-8');

  // Step 1: Parse JSON (checks syntax)
  let doc;
  try {
    doc = JSON.parse(content);
  } catch (e) {
    console.error(`JSON PARSE ERROR in ${file}: ${e.message}`);
    errors++;
    continue;
  }

  // Step 2: Validate as OpenAPI using swagger-parser
  try {
    await SwaggerParser.validate(structuredClone(doc));
    console.log(`OK: ${file}`);
  } catch (e) {
    console.error(`OPENAPI VALIDATION ERROR in ${file}: ${e.message}`);
    errors++;
  }
}

console.log(`\nValidated ${files.length} files, ${errors} errors.`);
if (errors > 0) {
  process.exit(1);
}
