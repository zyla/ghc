test('T10858',
     [compiler_stats_num_field('bytes allocated',
          [ (wordsize(64), 641075800, 8) ]),
      only_ways(['normal'])
      ],
     compile,
     ['-O'])
