VOWEL     = %w(a e i o u y)
DOUBLE    = %w(bb dd ff gg mm nn pp rr tt)
LI_ENDING = %w(c d e g h k m n r t)

def prepare(str)
  str.downcase
end

#TODO: handle y
def vowel?(char)
  VOWEL.include? char
end
