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

  def test_extracts_workspace_and_pane_ids_from_herdr_creation_output
    output = JSON.generate(
      "result" => {
        "workspace" => { "workspace_id" => "w1B" },
        "root_pane" => { "pane_id" => "w1B:p1" }
      }
    )

    assert_equal ["w1B", "w1B:p1"], @linear_work.send(:herdr_creation_ids, output)
  end

  def test_rejects_herdr_creation_output_without_both_ids
    output = JSON.generate("result" => { "workspace" => { "workspace_id" => "w1B" } })

    assert_nil @linear_work.send(:herdr_creation_ids, output)
  end

  def test_rejects_invalid_herdr_creation_json
    assert_nil @linear_work.send(:herdr_creation_ids, "not json")
  end
end
