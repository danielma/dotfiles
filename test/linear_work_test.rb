require "minitest/autorun"
load File.expand_path("../bin/linear-work", __dir__)

class LinearWorkTest < Minitest::Test
  def setup
    @linear_work = LinearWork.new([])
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

  def test_builds_herdr_worktree_command
    options = @linear_work.instance_variable_get(:@options)
    options[:base] = "origin/main"
    options[:path] = "/tmp/cca-123"

    command = @linear_work.send(:herdr_worktree_command, "daniel/cca-123", "CCA-123")

    assert_equal [
      "herdr", "worktree", "create",
      "--cwd", Dir.pwd,
      "--branch", "daniel/cca-123",
      "--label", "CCA-123",
      "--focus",
      "--base", "origin/main",
      "--path", "/tmp/cca-123"
    ], command
  end

  def test_builds_herdr_agent_start_command
    command = @linear_work.send(:herdr_agent_start_command, "cca-123", "w1B:p1")

    assert_equal [
      "herdr", "agent", "start", "cca-123",
      "--kind", "claude",
      "--pane", "w1B:p1",
      "--timeout", "300000",
      "--", "--permission-mode", "plan", "--model", "opus"
    ], command
  end

  def test_creates_worktree_starts_claude_and_submits_prompt
    status = Object.new
    status.define_singleton_method(:success?) { true }
    creation_output = JSON.generate(
      "result" => {
        "workspace" => { "workspace_id" => "w1B" },
        "root_pane" => { "pane_id" => "w1B:p1" }
      }
    )
    responses = [
      [creation_output, "", status],
      [JSON.generate("result" => {}), "", status],
      [JSON.generate("result" => {}), "", status]
    ]
    commands = []
    capture = lambda do |*command|
      commands << command
      responses.shift
    end

    output, = capture_io do
      Open3.stub(:capture3, capture) do
        @linear_work.stub(:require_command, nil) do
          @linear_work.send(:start_herdr_worktree, "daniel/cca-123", "Issue prompt", "CCA-123")
        end
      end
    end

    assert_equal "Started CCA-123 in Herdr workspace w1B\n", output
    assert_equal ["herdr", "agent", "prompt", "cca-123", "Issue prompt"], commands.last
    assert_empty responses
  end
end
