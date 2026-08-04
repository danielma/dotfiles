require "minitest/autorun"
load File.expand_path("../bin/linear-work", __dir__)

class LinearWorkTest < Minitest::Test
  def setup
    @linear_work = LinearWork.new([])
  end

  def test_uses_percent_encoded_path_as_worktree_id
    worktree_id = "%2FUsers%2Fdanielma%2F.supacode%2Frepos%2Fapp%2Fcca-123%2F"

    assert_equal worktree_id, @linear_work.send(:worktree_id_from_output, "#{worktree_id}\n")
  end

  def test_rejects_ambiguous_worktree_creation_output
    output = "%2Fpath%2Fone%2F\n%2Fpath%2Ftwo%2F\n"

    assert_nil @linear_work.send(:worktree_id_from_output, output)
  end

  def test_worktree_readiness_requires_an_exact_id_match
    worktree_id = "%2Fpath%2Fcca-123%2F"
    output = "%2Fpath%2Fcca-123-old%2F\tunpinned\n"

    refute @linear_work.send(:worktree_list_includes?, output, worktree_id)
  end
end
