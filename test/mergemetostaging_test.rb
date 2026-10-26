require "fileutils"
require "minitest/autorun"
require "open3"
require "tmpdir"

class MergeMeToStagingTest < Minitest::Test
  SCRIPT = File.expand_path("../bin/mergemetostaging", __dir__)

  def setup
    @tmpdir = Dir.mktmpdir
    @origin = File.join(@tmpdir, "origin.git")
    @repo = File.join(@tmpdir, "repo")

    git(@tmpdir, "init", "--bare", @origin)
    git(@tmpdir, "init", "--initial-branch=main", @repo)
    git(@repo, "config", "user.name", "Test User")
    git(@repo, "config", "user.email", "test@example.com")
    File.write(File.join(@repo, "README.md"), "initial\n")
    git(@repo, "add", "README.md")
    git(@repo, "commit", "-m", "Initial commit")
    git(@repo, "remote", "add", "origin", @origin)
    git(@repo, "switch", "-c", "staging")
    git(@repo, "push", "--set-upstream", "origin", "staging")
    git(@repo, "switch", "-c", "feature")
    File.write(File.join(@repo, "feature.txt"), "feature\n")
    git(@repo, "add", "feature.txt")
    git(@repo, "commit", "-m", "Add feature")
    git(@repo, "branch", "-D", "staging")
  end

  def teardown
    FileUtils.remove_entry(@tmpdir)
  end

  def test_merges_current_branch_to_staging_and_pushes_it
    _output, status = Open3.capture2e(SCRIPT, chdir: @repo)

    assert status.success?
    assert_equal "staging", git(@repo, "branch", "--show-current").strip
    assert_equal git(@repo, "rev-parse", "feature"), git(@repo, "rev-parse", "origin/staging")
  end

  def test_refuses_to_run_with_uncommitted_changes
    File.write(File.join(@repo, "dirty.txt"), "dirty\n")

    output, status = Open3.capture2e(SCRIPT, chdir: @repo)

    refute status.success?
    assert_includes output, "Nope! You have changes"
    assert_equal "feature", git(@repo, "branch", "--show-current").strip
  end

  private

  def git(directory, *arguments)
    output, status = Open3.capture2e("git", *arguments, chdir: directory)
    raise output unless status.success?

    output
  end
end
