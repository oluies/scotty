import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

// The pages scripts/readme-to-docs.mjs generates from README.md, in reading order.
const sidebars: SidebarsConfig = {
  docsSidebar: [
    'index',
    'getting-started',
    'density',
    {
      type: 'category',
      label: 'Algorithms',
      link: {type: 'doc', id: 'algorithms/index'},
      items: ['algorithms/shor', 'algorithms/resources', 'algorithms/references'],
    },
    'gate-fusion',
    'scala3-port',
    'downstream',
  ],
};

export default sidebars;
