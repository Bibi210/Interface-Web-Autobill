import React, { useState } from 'react';

function TestBackend() {
  const [code, setCode] = useState('');
  const [response, setResponse] = useState('');

  function handleChange(event) {
    setCode(event.target.value);
  }

  function handleClick() {
    console.log("ready to send");
    fetch('http://localhost:3001/api/run-code', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json; charset = UTF-8'

      },
      body: JSON.stringify({ code: code }) 
    })
      .then(res => res.json())
      .then(data => setResponse(data.result))
      .catch(error => console.error(error));
      console.log("recieved result");

  }

  return (
    <div>
      <textarea value={code} onChange={handleChange} />
      <button onClick={handleClick}>运行代码</button>
      <div>{response}</div>
    </div>
  );
}

export default TestBackend;
