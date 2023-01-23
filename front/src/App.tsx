import { useState } from 'react'
import CodeEditor from '../components/CodeEditor'

function App() {
  const [count, setCount] = useState(0)

  return (
    <>
    <header className='container'>
      <nav>
      <div className='branding'>
        <img src="/logo.png" alt="" />
        <h1>Autobill</h1>
      </div>
      </nav>
    </header>
     <main>
      <section>
        <CodeEditor />
      </section>
      <section>
        <aside>
          <span className='output'>
            Output
          </span>
        </aside>
      </section>
    </main>
    </>
   
  )
}

export default App
