// Split each topic x truth x stance cell at random into NQ and LQ halves
function assignQualityBalanced(stimuliArray) {
  // Group by topic, truth and stance
  const groups = {};
  stimuliArray.forEach(s => {
    const key = s.topic + '|' + s.truth + '|' + s.stance;
    if (!groups[key]) groups[key] = [];
    groups[key].push(s);
  });

  const result = [];
  Object.keys(groups).forEach(key => {
    // Shuffle within the cell so quality assignment is random
    const cell = jsPsych.randomization.shuffle(groups[key]);
    const half = Math.ceil(cell.length / 2);  // Extra goes to NQ if odd
    cell.forEach((s, i) => {
      const quality = i < half ? 'NQ' : 'LQ';
      result.push(Object.assign({}, s, {
        quality: quality,
        text: quality === 'LQ' ? s.text_LQ : s.text_NQ,
        wordcount: quality === 'LQ' ? s.wordcount_LQ : s.wordcount_NQ
      }));
    });
  });

  return result;
}

// Draw perCell stimuli at random from each topic x truth x stance x quality cell
function sampleBalancedFromCells(qualityAssigned, perCell) {
  const groups = {};
  qualityAssigned.forEach(s => {
    const key = s.topic + '|' + s.truth + '|' + s.stance + '|' + s.quality;
    if (!groups[key]) groups[key] = [];
    groups[key].push(s);
  });

  const out = [];
  Object.keys(groups).forEach(key => {
    const n = Math.min(perCell, groups[key].length);
    const drawn = jsPsych.randomization.sampleWithoutReplacement(groups[key], n);
    drawn.forEach(s => out.push(s));
  });
  return out;
}

// Shuffle the stimuli and assign each one a source from the same topic
function prepareRandomizedStimuli(stimuli, sources) {
  // Group sources by topic
  const sourcesByTopic = {};
  sources.forEach(source => {
    if (!sourcesByTopic[source.topic]) {
      sourcesByTopic[source.topic] = [];
    }
    sourcesByTopic[source.topic].push(source.source);
  });
  
  // Randomize source order within each topic
  for (let topic in sourcesByTopic) {
    sourcesByTopic[topic] = jsPsych.randomization.shuffle(sourcesByTopic[topic]);
  }
  
  // Index of the next source to use for each topic
  const sourceIndices = {};
  for (let topic in sourcesByTopic) {
    sourceIndices[topic] = 0;
  }
  
  // Randomize the order of stimuli
  const shuffledStimuli = jsPsych.randomization.shuffle(stimuli);
  
  // Match each stimulus with a source from the matching topic
  const preparedStimuli = shuffledStimuli.map(stimulus => {
    const topic = stimulus.topic;
    
    // Reuse sources if a topic runs out
    if (!sourcesByTopic[topic] || sourceIndices[topic] >= sourcesByTopic[topic].length) {
      console.warn(`Warning: Not enough sources for topic "${topic}". Reusing sources.`);
      sourceIndices[topic] = 0;
    }
    
    // Get the next unused source for this topic
    const assignedSource = sourcesByTopic[topic][sourceIndices[topic]];
    sourceIndices[topic]++;
    
    // Copy the stimulus and add the assigned source
    return Object.assign({}, stimulus, {
      source: assignedSource,
      author: assignedSource,
      avatar: assignedSource.charAt(0),
      content: stimulus.text,
      true_value: stimulus.truth === 'true'
    });
  });
  
  return preparedStimuli;
}
