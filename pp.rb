require  "pp"
require "json"
pp JSON.parse File.read ARGV[0]
