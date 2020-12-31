local helpers = require('test.helpers')
local eq = helpers.eq
local has_no_errors = helpers.has_no_errors

describe('src/testdir/test_vim9_assign.vim', function()
  describe('Test_assignment_bool', function()
    it('should handle boolean assignments', function()
      has_no_errors [[
        var bool1: bool = true
        assert_equal(v:true, bool1)
        var bool2: bool = false
        assert_equal(v:false, bool2)
      ]]
    end)

    it('should auto convert numbers to bools', function()
      has_no_errors [[
        var bool3: bool = 0
        assert_equal(false, bool3)
        var bool4: bool = 1
        assert_equal(true, bool4)
      ]]
    end)

    pending('convert binary expressions', function()
      has_no_errors [[
        var bool5: bool = 1 && true
        assert_equal(true, bool5)
        var bool6: bool = 0 && 1
        assert_equal(false, bool6)
        var bool7: bool = 0 || 1 && true
        assert_equal(true, 5)
      ]]
    end)
  end)

  describe('tests that confuse me', function()
    pending('currently does not auto convert subtypes', function()
      local _ = [[
        def MyFunc(): bool
            var my_bool: bool = 1
            var my_list: list<bool> = [true, 1]

            echo my_bool
            echo my_list[0]

            return false

            # var other_list: list<bool> = my_list
        enddef
      ]]
    end)
  end)
end)

--[===[
def Test_assignment_bool()
  var lines = < < trim END
    vim9script
    def GetFlag(): bool
      var flag: bool = 1
      return flag
    enddef
    var flag: bool = GetFlag()
    assert_equal(true, flag)
    flag = 0
    assert_equal(false, flag)
    flag = 1
    assert_equal(true, flag)
    flag = 1 || true
    assert_equal(true, flag)
    flag = 1 && false
    assert_equal(false, flag)
  END
  CheckScriptSuccess(lines)
  CheckDefAndScriptFailure(['var x: bool = 2'], 'E1012:')
  CheckDefAndScriptFailure(['var x: bool = -1'], 'E1012:')
  CheckDefAndScriptFailure(['var x: bool = [1]'], 'E1012:')
  CheckDefAndScriptFailure(['var x: bool = {}'], 'E1012:')
  CheckDefAndScriptFailure(['var x: bool = "x"'], 'E1012:')
enddef
--]===]
