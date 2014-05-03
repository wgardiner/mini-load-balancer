require 'rubygems'
require 'sinatra'

get '/' do
    "OK, no problema!"
end

get '/tetra/:xvar' do |x|
    "#{x.to_i ** x.to_i}"
end
