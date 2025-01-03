# Programming Languages, Dan Grossman
# Section 7: Introduction to Ruby

class A
  def initialize(f = 0)
    @foa = f
  end

  def get_foa1
    @foa
  end
  # private/protected method can only be called in methods of the same class/subclass
  def use_private_method #when the method is all about one same object, private method can cover
    @foa + get_foa3
  end

  def use_protected_method #when the method involves another object of the same class, say OBJECT B, then B's method have to be protected or public
    b = A.new
    @foa + b.get_foa2 + get_foa3 + b.get_foa1
  end
  
protected
  def get_foa2
    @foa
  end
  
private
  def get_foa3
    @foa
  end
end
  

class B
  def initialize(f = 1)
    @fob = f
  end
  #where x = A.new
  #public method can be used in methods of any class
  def use_public_method x
    @fob + x.get_foa1
  end

  def use_protected_method x #raise an error
    @fob + x.get_foa2
  end

  def get_fob
    @fob += A.new.get_foa #only way to get an object's instance variable is through its method
    # @fob += A.new.@foa WRONG
    @fob
  end
end

class C
  Class_Variable = 10 # class constant

  def self.setbar #self.xx defines a class method
    @@bar = 0
  end

  def setbar 
    @@bar += 1 # @@x defines a class instance variable
  end

  def initialize(f = 0)
    @foo = f
  end

  def setfoo
    @foo += 1
  end
  
  def get_foo
    @foo
  end

  def get_bar
    @@bar
  end

  def get_object
    self
  end
end


      
class MyRational

  def initialize(num,den=1) # second argument has a default
    if den == 0
      raise "MyRational received an inappropriate argument"
    elsif den < 0 # notice non-english word elsif
      @num = - num # fields created when you assign to them
      @den = - den
    else
      @num = num # semicolons optional to separate expressions on different lines
      @den = den
    end
    reduce # i.e., self.reduce() but private so must write reduce or reduce()
  end

  def to_s 
    ans = @num.to_s
    if @den != 1 # everything true except false _and_ nil objects
      ans += "/"
      ans += @den.to_s 
    end
    ans
  end

  def to_s2 # using some unimportant syntax and a slightly different algorithm
    dens = ""
    dens = "/" + @den.to_s if @den != 1
    @num.to_s + dens
  end

  def to_s3 # using things like Racket's quasiquote and unquote
    "#{@num}#{if @den==1 then "" else "/" + @den.to_s end}"
  end

  def add! r # mutate self in-place
    a = r.num # only works b/c of protected methods below
    b = r.den # only works b/c of protected methods below
    c = @num
    d = @den
    @num = (a * d) + (b * c)
    @den = b * d
    reduce
    self # convenient for stringing calls
  end

  # a functional addition, so we can write r1.+ r2 to make a new rational
  # and built-in syntactic sugar will work: can write r1 + r2
  def + r
    ans = MyRational.new(@num,@den)
    ans.add! r
    ans
  end
    
protected  
  # there is very common sugar for this (attr_reader)
  # the better way:
  # attr_reader :num, :den
  # protected :num, :den
  # we do not want these methods public, but we cannot make them private
  # because of the add! method above
  def num
    @num
  end
  def den
    @den
  end

private
  def gcd(x,y) # recursive method calls work as expected
    if x == y
      x
    elsif x < y
      gcd(x,y-x)
    else
      gcd(y,x)
    end
  end

  def reduce
    if @num == 0
      @den = 1
    else
      d = gcd(@num.abs, @den) # notice method call on number
      @num = @num / d
      @den = @den / d
    end
  end
end
