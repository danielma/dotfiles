module.exports = function(results, context) {
  if (results.length > 1) {
    throw new Error("expected only one result")
  }

  return results[0].output
};
