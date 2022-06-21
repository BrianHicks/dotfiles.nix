# stuff that I notice is missing from my tools, mostly, rather than an attempt to
# be exhaustive.

{ a: 1, b: 2 }.each do |k, v|
  puts "#{k} #{v}"
end

{ c: 1, d: 2 }.each { |k, v| puts "#{k} #{v}" }

let(:foo) { :bar }

context "some rspec thing" do
  describe "a component" do
    it "should detect these names"
  end
end

class Person < T::Struct
  prop :name, String
  const :dob, TimeWithZone
end

def method_with_keyword_arguments(required, keyword: nil, optional = nil) do
  fail "whoops"
end

class Foo
  attr_reader :reader
  attr_writer :writer
  attr_accessor :accessor
end
