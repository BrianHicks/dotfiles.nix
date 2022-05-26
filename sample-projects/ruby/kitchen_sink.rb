# stuff that I notice is missing from my tools, mostly, rather than an attempt to
# be exhaustive.

{ a: 1, b: 2 }.each do |k, v|
  puts "#{k} #{v}"
end

{ c: 1, d: 2 }.each { |k, v| puts "#{k} #{v}" }

let(:foo) { :bar }
