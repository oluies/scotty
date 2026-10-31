import fs from 'node:fs';
import path from 'node:path';
import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

// Docs for the oluies fork of Scotty, published by .github/workflows/pages.yml to
// https://oluies.github.io/scotty/. The original project's site, www.entangled.xyz/scotty/, is gone.
//
// The pages are generated from README.md by scripts/readme-to-docs.mjs, so the README stays the one
// source and its examples, which ReadmeExampleSpec runs, are the ones shown here. The Scaladoc API
// reference is built by sbt in the same workflow and copied into static/api/ before this build.

// The latest release, read from the README's dependency line, which names the version to depend
// on. build.sbt is no use here: between releases it carries the next snapshot, which has no release
// page to link to.
const readme = fs.readFileSync(path.join(__dirname, '..', 'README.md'), 'utf8');
const released = /"io\.github\.oluies" %% "scotty" % "([^"]+)"/.exec(readme)?.[1];

const config: Config = {
  title: 'Scotty',
  tagline: 'Quantum computing in Scala: a state-vector simulator, density matrices and quantum channels.',
  favicon: 'img/logo.svg',

  url: 'https://oluies.github.io',
  baseUrl: '/scotty/',
  trailingSlash: true,

  organizationName: 'oluies',
  projectName: 'scotty',

  onBrokenLinks: 'throw',
  onBrokenAnchors: 'throw',

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      {
        docs: {
          sidebarPath: './sidebars.ts',
          // Docs ARE the site: /scotty/<page>/
          routeBasePath: '/',
          editUrl: 'https://github.com/oluies/scotty/edit/pctc/README.md',
        },
        blog: false,
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  markdown: {
    // The pages come from a README full of `|x>`, `<=` and braces, which MDX would read as JSX;
    // plain CommonMark for .md files keeps them text.
    format: 'detect',
    hooks: {
      onBrokenMarkdownLinks: 'throw',
    },
  },

  themeConfig: {
    metadata: [
      {name: 'keywords', content: 'Scala, Scala 3, quantum computing, simulator, density matrix, quantum channel, Shor, Grover, Scala.js'},
      {name: 'description', content: 'Scotty: a quantum computing framework for Scala 3, with a state-vector simulator, density matrices, quantum channels, Shor and Grover.'},
    ],
    colorMode: {
      respectPrefersColorScheme: true,
    },
    navbar: {
      title: 'Scotty',
      logo: {
        alt: 'Scotty',
        src: 'img/logo.svg',
      },
      items: [
        {type: 'docSidebar', sidebarId: 'docsSidebar', position: 'left', label: 'Docs'},
        // static/api/ is the Scaladoc the Pages workflow builds; a plain href so the link checker,
        // which runs before that copy exists locally, does not treat it as a route.
        {href: 'pathname:///scotty/api/index.html', label: 'API', position: 'left'},
        {href: 'https://github.com/oluies/scotty/releases', label: released ? `Releases (${released})` : 'Releases', position: 'right'},
        {href: 'https://github.com/oluies/scotty', label: 'GitHub', position: 'right'},
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Docs',
          items: [
            {label: 'Introduction', to: '/'},
            {label: 'Getting started', to: '/getting-started/'},
            {label: 'Algorithms', to: '/algorithms/'},
          ],
        },
        {
          title: 'Built on Scotty',
          items: [
            {label: 'pctc', href: 'https://oluies.github.io/pctc/'},
            {label: 'brokkr', href: 'https://oluies.github.io/brokkr/'},
          ],
        },
        {
          title: 'Source',
          items: [
            {label: 'oluies/scotty', href: 'https://github.com/oluies/scotty'},
            {label: 'Original project', href: 'https://github.com/entangled-xyz/scotty'},
          ],
        },
      ],
      copyright: 'Scotty is available under the Apache 2.0 License. Built with Docusaurus.',
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ['java', 'scala', 'bash'],
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
