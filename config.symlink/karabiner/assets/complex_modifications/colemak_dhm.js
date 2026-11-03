// JavaScript must be written in ECMAScript 5.1.

function main() {
  var layout = [
    ['e', 'f'],
    ['r', 'p'],
    ['t', 'b'],
    ['y', 'j'],
    ['u', 'l'],
    ['i', 'u'],
    ['o', 'y'],
    ['p', 'semicolon'],
    ['s', 'r'],
    ['d', 's'],
    ['f', 't'],
    ['h', 'm'],
    ['j', 'n'],
    ['k', 'e'],
    ['l', 'i'],
    ['semicolon', 'o'],
    ['v', 'd'],
    ['b', 'v'],
    ['n', 'k'],
    ['m', 'h'],
  ]

  return {
    description: 'Colemak Mod-DHm (matrix / ortho keyboards)',
    manipulators: layout.map(function (keys) {
      return {
        type: 'basic',
        from: {
          key_code: keys[0],
          modifiers: { optional: ['any'] },
        },
        to: [{ key_code: keys[1] }],
      }
    }),
  }
}

main()
