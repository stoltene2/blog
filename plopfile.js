/*
 * To use Plop, you need to have it installed
 *  > yarn
 *  > yarn run plop
 *
 * Creating new Generators
 *
 *   Read https://github.com/amwmedia/plop for terminology
 *
 *   Templates are located in plop/templates.
 *
 *   posts/<date>-<post-title/
 *   ├── images
 *   │   └── .gitignore
 *   ├── index.markdown
 *   └── partials
 *        └── .gitignore
 *
 */

var path = require('path'),
    fs   = require('fs');

module.exports = function (plop) {
  function promptDate() {
    return {
      type: 'input',
      name: 'postDate',
      message: 'Which date is this post for?',
      validate: function (value) {
        if ((/\d{4}-\d{2}-\d{2}/).test(value)) { return true; }
        return 'date is required';
      }
    };
  }

  function promptPostTitle() {
    return {
      type: 'input',
      name: 'postTitle',
      message: 'Title of blog post',
      validate: function (value) {
        if ((/.+/).test(value)) { return true; }
        return 'Title is required';
      }
    };

  }

  function extendAnswers(answers) {
    answers.postPath = plop.renderString('{{postDate}}-{{dashCase postTitle}}', answers);
  }

  plop.setGenerator('Post', {
    description: 'Create new blog post structure',
    prompts: [
      promptDate(),
      promptPostTitle()
    ],
    actions: function(answers) {
      extendAnswers(answers);
      return [
        {
          type: 'add',
          path: 'posts/{{postPath}}/index.markdown',
          templateFile: 'plop/templates/index.markdown.tmpl'
        },
        {
          type: 'add',
          path: 'posts/{{postPath}}/images/.gitignore'
        },
        {
          type: 'add',
          path: 'posts/{{postPath}}/partials/.gitignore'
        }
      ];
    }
  });
};
