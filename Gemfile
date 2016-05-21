source 'https://rubygems.org'

require 'json'
require 'open-uri'
versions = JSON.parse(open('https://pages.github.com/versions.json').read)

# GitHub Pages environment
gem 'github-pages', versions['github-pages']

# Tools
gem 'rake'
gem 'travis'
gem 'html-proofer'
gem 'scss_lint', require: false
gem 'rubocop'
gem 'mdl'
