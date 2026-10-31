// Generates website/docs/ from the repository's README.md.
//
// The README is the one source: ReadmeExampleSpec runs its examples, and every PR keeps it current.
// Copying it into hand-written pages would give the site a second copy that drifts, so instead the
// build splits it here, by heading, into the pages listed below, demotes each page's headings one
// level, and rewrites the README's in-page anchors into links between the pages. docs/ is
// generated, git-ignored, and rebuilt by `npm start` and `npm run build`.
//
// No dependencies: plain Node, so the step costs nothing to install.

import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

const here = path.dirname(fileURLToPath(import.meta.url));
const website = path.resolve(here, '..');
const readme = fs.readFileSync(path.resolve(website, '..', 'README.md'), 'utf8');
const out = path.join(website, 'docs');
const repo = 'https://github.com/oluies/scotty';

// Each page takes the README sections whose headings it lists, in order. A section runs from its
// heading to the next heading of the same or a higher level, so taking a `##` section takes its
// `###` subsections too. `title` is the page's own heading; the first section's heading is dropped
// in its favour, unless `keepHeadings` keeps every section's heading as a `##` (a page made of
// several sections whose headings are link targets).
const pages = [
  {file: 'index.md', title: 'Scotty', slug: '/', label: 'Introduction', sections: ['# Scotty: Quantum Computing in Scala'], stopAt: '## Building'},
  {file: 'getting-started.md', title: 'Getting started', sections: ['## Building']},
  {file: 'density.md', title: 'Density matrices and quantum channels', label: 'Density matrices', sections: ['## Density matrices and quantum channels']},
  {file: 'algorithms/index.md', title: 'Algorithms', sections: ['## Algorithms'], exclude: ['### Shor for any odd modulus', '### Shor on `L + 1` qubits', '### Pretending to factor', '### What the circuits cost', '### Shor beyond 15, and without a quantum computer']},
  {file: 'algorithms/shor.md', title: "Shor's algorithm beyond 15", label: "Shor's algorithm", keepHeadings: true, sections: ['### Shor for any odd modulus', '### Shor on `L + 1` qubits', '### Pretending to factor']},
  {file: 'algorithms/resources.md', title: 'What the circuits cost', label: 'Resource counts', sections: ['### What the circuits cost']},
  {file: 'algorithms/references.md', title: 'Shor beyond 15, and without a quantum computer', label: 'References', sections: ['### Shor beyond 15, and without a quantum computer']},
  {file: 'gate-fusion.md', title: 'Gate fusion', sections: ['## Gate fusion']},
  {file: 'scala3-port.md', title: 'Notes on the Scala 3 port', label: 'Scala 3 port', sections: ['## Notes on the Scala 3 port']},
  {file: 'downstream.md', title: 'Downstream projects', sections: ['## Downstream projects', '## Contributing', '## License']},
];

// README sections deliberately left off the site. Every other `##` section has to be on a page, or
// the build fails: a section added to the README and forgotten here would otherwise vanish from the
// site with the build still passing. Getting Started exists to send README readers to the site, and
// the site's own Getting started page (Building and Releases) already says how to install.
const skip = ['## Getting Started'];

// Where each README anchor lives on the site: a page, and an anchor on it or none.
const anchors = {
  'what-this-fork-adds': ['index.md', 'what-this-fork-adds'],
  'releases': ['getting-started.md', 'releases'],
  'density-matrices-and-quantum-channels': ['density.md', ''],
  'algorithms': ['algorithms/index.md', ''],
  'shor-for-any-odd-modulus': ['algorithms/shor.md', 'shor-for-any-odd-modulus'],
  'shor-on-l--1-qubits': ['algorithms/shor.md', 'shor-on-l--1-qubits'],
  'pretending-to-factor': ['algorithms/shor.md', 'pretending-to-factor'],
  'what-the-circuits-cost': ['algorithms/resources.md', ''],
  'shor-beyond-15-and-without-a-quantum-computer': ['algorithms/references.md', ''],
  'gate-fusion': ['gate-fusion.md', ''],
  'downstream-projects': ['downstream.md', ''],
};

// Split into sections, ignoring `#` lines inside fenced code.
const lines = readme.split('\n');
const sections = [];
let fenced = false;
lines.forEach((line, i) => {
  if (/^```/.test(line)) fenced = !fenced;
  const m = !fenced && /^(#{1,6}) /.exec(line);
  if (m) sections.push({heading: line.trim(), level: m[1].length, start: i});
});

function sectionLines(heading, exclude = [], stopAt) {
  const index = sections.findIndex((s) => s.heading === heading);
  if (index < 0) throw new Error(`README has no section "${heading}"`);
  const {level, start} = sections[index];
  const after = sections.slice(index + 1);
  const end = (after.find((s) => s.level <= level || s.heading === stopAt) ?? {start: lines.length}).start;
  const skipped = new Set();
  for (const ex of exclude) {
    const j = sections.findIndex((s) => s.heading === ex);
    if (j < 0) throw new Error(`README has no section "${ex}" to exclude`);
    const exEnd = (sections.slice(j + 1).find((s) => s.level <= sections[j].level) ?? {start: lines.length}).start;
    for (let k = sections[j].start; k < exEnd; k++) skipped.add(k);
  }
  const covered = [];
  for (let k = start; k < end; k++) if (!skipped.has(k)) covered.push(k);
  return {level, lines: lines.slice(start, end).filter((_, k) => !skipped.has(start + k)), covered};
}

function relative(from, to) {
  const rel = path.posix.relative(path.posix.dirname(from), to);
  return rel.startsWith('.') ? rel : `./${rel}`;
}

// Every README line a page takes, to check that no `##` section is left out.
const takenLines = new Set();

for (const page of pages) {
  const body = [];
  page.sections.forEach((heading, n) => {
    const {lines: taken, covered} = sectionLines(heading, page.exclude, page.stopAt);
    covered.forEach((k) => takenLines.add(k));
    // The page's own title replaces the first section's heading unless keepHeadings is set; the
    // headings that remain are shifted so that the shallowest of them becomes `##`.
    const kept = n === 0 && !page.keepHeadings ? taken.slice(1) : taken;
    const levels = [];
    let inFence = false;
    for (const line of kept) {
      if (/^```/.test(line)) inFence = !inFence;
      const m = !inFence && /^(#{1,6}) /.exec(line);
      if (m) levels.push(m[1].length);
    }
    const shift = levels.length ? Math.min(...levels) - 2 : 0;
    inFence = false;
    for (const line of kept) {
      if (/^```/.test(line)) inFence = !inFence;
      const m = !inFence && /^(#{1,6}) (.*)$/.exec(line);
      body.push(m ? `${'#'.repeat(m[1].length - shift)} ${m[2]}` : line);
    }
  });

  let text = body.join('\n').trim() + '\n';
  text = text.replace(/\]\(#([^)]+)\)/g, (whole, anchor) => {
    const target = anchors[anchor];
    if (!target) throw new Error(`${page.file}: no page for README anchor #${anchor}`);
    const [file, frag] = target;
    const link = file === page.file ? '' : relative(page.file, file);
    return `](${link}${frag ? `#${frag}` : ''})`;
  });
  // The README's links to files in the repository become links to them on GitHub.
  text = text.replace(/\]\((?!https?:|#|\.{0,2}\/|[^)]*\.md)([^)]+)\)/g, (_, file) => `](${repo}/blob/pctc/${file})`);

  const front = ['---', `title: ${JSON.stringify(page.title)}`];
  if (page.label) front.push(`sidebar_label: ${JSON.stringify(page.label)}`);
  if (page.slug) front.push(`slug: ${page.slug}`);
  front.push(`custom_edit_url: ${repo}/edit/pctc/README.md`, '---', '');

  const target = path.join(out, page.file);
  fs.mkdirSync(path.dirname(target), {recursive: true});
  fs.writeFileSync(target, front.join('\n') + '\n' + text);
}

const missing = sections.filter((s) => s.level === 2 && !takenLines.has(s.start) && !skip.includes(s.heading));
if (missing.length) {
  throw new Error(`README sections on no page and not in skip: ${missing.map((s) => s.heading).join(', ')}`);
}
for (const heading of skip) {
  if (!sections.some((s) => s.heading === heading)) throw new Error(`skip names "${heading}", which the README no longer has`);
}

console.log(`readme-to-docs: wrote ${pages.length} pages to ${path.relative(process.cwd(), out) || 'docs'}`);
