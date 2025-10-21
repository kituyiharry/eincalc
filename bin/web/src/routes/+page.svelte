<script>
  import { Canvas, Layer } from 'svelte-canvas';
  // TODO: standardize controller so that we can type it
  import { default as controller } from '$lib/index';

  let refresh = 0;

  const CELL_WIDTH       = 120;
  const CELL_HEIGHT      = 30;
  const HEADER_HEIGHT    = 30;
  const ROW_HEADER_WIDTH = 60;

  // needed to render the editing cell absolutelt
  const DRAWER           = 57; 

  // Scroll computation
  let scrollX = 0;
  let scrollY = 0;
  let cellData = {}

  // cell selection
  let selectedCell   = null;
  let selectionStart = null;
  let selectionEnd   = null;
  let isDragging     = false;

  // editing support via floating input 
  let editor
  let editingCell = null;
  let editValue = '';

  // prevent refetching data by caching values in a set
  let visibleCells = new Set();

  $: editorStyle = editingCell ? {
    position: 'absolute',
    left:    `${editingCell.col * CELL_WIDTH  - scrollX + ROW_HEADER_WIDTH + DRAWER}px`,
    top:     `${editingCell.row * CELL_HEIGHT - scrollY + HEADER_HEIGHT}px`,
    width:   `${CELL_WIDTH}px`,
    height:  `${CELL_HEIGHT}px`,
    border:  '2px solid #1976d2',
    padding: '4px',
    fontSize: '10px',
    fontFamily: 'sans-serif',
    outline: 'none',
    zIndex: '10'
  } : { display: 'none' };

  function getColumnLabel(col) {
    let label = [];
    let num   = col;
    while (num >= 0) {
      label.push(String.fromCharCode(65 + (num % 26)));
      num = Math.floor(num / 26) - 1;
    }
    return label.join('');
  }
  
  function handleWheel(e) {
    e.preventDefault();
    scrollX = Math.max(0, scrollX + e.deltaX);
    scrollY = Math.max(0, scrollY + e.deltaY);
  }

  // Callback that gets called when a cell comes into view
  function onCellVisible(row, col) {
      const key = `${row},${col}`;
      if (!visibleCells.has(key)) {
          //  console.log(`Cell (${row}, ${col}) came into view - fetch data here`);
          //  actual async data fetching
          if (!cellData[key]) {
              cellData[key] = controller.myLib.get(row, col);
          }
          visibleCells.add(key);
      }
  }

  function getCellFromPosition(x, y) {
      if (x < ROW_HEADER_WIDTH || y < HEADER_HEIGHT) return null;

      const col = Math.floor((x - ROW_HEADER_WIDTH + scrollX) / CELL_WIDTH);
      const row = Math.floor((y - HEADER_HEIGHT + scrollY) / CELL_HEIGHT);

      return { row, col };
  }

  function getSelectionBounds() {
      if (!selectionStart || !selectionEnd) return null;

      return {
          startRow: Math.min(selectionStart.row, selectionEnd.row),
          endRow:   Math.max(selectionStart.row, selectionEnd.row),
          startCol: Math.min(selectionStart.col, selectionEnd.col),
          endCol:   Math.max(selectionStart.col, selectionEnd.col)
      };
  }


  function isCellSelected(row, col) {
      const bounds = getSelectionBounds();
      if (!bounds) return false;

      return row >= bounds.startRow && row <= bounds.endRow &&
          col >= bounds.startCol && col <= bounds.endCol;
  }

  // Dragging and selection
  function handleMouseDown(e) {
      const rect = e.target.getBoundingClientRect();
      const x = e.clientX - rect.left;
      const y = e.clientY - rect.top;

      const cell = getCellFromPosition(x, y);
      if (cell) {
          selectedCell = cell;
          selectionStart = cell;
          selectionEnd = cell;
          isDragging = true;
          editingCell = null;
      }
  }

  function handleMouseMove(e) {
      if (!isDragging) return;

      const rect = e.target.getBoundingClientRect();
      const x = e.clientX - rect.left;
      const y = e.clientY - rect.top;

      const cell = getCellFromPosition(x, y);
      if (cell && (cell.row !== selectionEnd?.row || cell.col !== selectionEnd?.col)) {
          selectionEnd = cell;
      }
      refresh++;
  }

  function handleMouseUp() {
      isDragging = false;
  }

  // function handleClick(e) {
  //     if (isDragging) return
  //     const rect = e.target.getBoundingClientRect();
  //     const x = e.clientX - rect.left;
  //     const y = e.clientY - rect.top;

  //     const cell = getCellFromPosition(x, y);
  //     if (cell) {
  //         const cellKey = `${cell.row},${cell.col}`;
  //         selectionStart = cell;
  //         selectionEnd = cell;
  //     }
  //     counter++;
  // }

  function handleDoubleClick(e) {
      const rect = e.target.getBoundingClientRect();
      const x = e.clientX - rect.left;
      const y = e.clientY - rect.top;

      const cell = getCellFromPosition(x, y);
      if (cell) {
          const cellKey = `${cell.row},${cell.col}`;
          editingCell = cell;
          selectionStart = cell;
          selectionEnd = cell;
          editValue = cellData[cellKey] || '';
          setTimeout(() => editor?.focus(), 0);
      }
      refresh++;
  }

  
  function handleEditorBlur() {
    if (editingCell) {
      const cellKey = `${editingCell.row},${editingCell.col}`;
      const editOut = editValue.trim();
      if (editOut.startsWith('=')) {
        const num = parseFloat(editOut.substring(1));  
        controller.myLib.gridaddnumber(editingCell.row, editingCell.col, num);
        console.log("was number");
      } else {
        controller.myLib.gridaddstring(editingCell.row, editingCell.col, editOut);
        console.log("was string");
      }
      visibleCells.delete(cellKey);
      //cellData[cellKey] = editValue;
      editingCell = null;
      refresh++;
    }
  }

  
  function handleEditorKeyDown(e) {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      handleEditorBlur();
    } else if (e.key === 'Escape') {
      editingCell = null;
      editValue = '';
    }
  }

  ;;

</script>

<!--<svelte:window />-->
<div class='h-[100vh] w-[100vw]'>

    <!--TODO: wheel and touch events may not work as well on layers - find out why ?? --->
    <input
        bind:this={editor}
        type="text"
        bind:value={editValue}
        on:blur={handleEditorBlur}
        on:keydown={handleEditorKeyDown}
        class="bg-black text-white text-xs"
        style={Object.entries(editorStyle).map(([k, v]) => `${k}: ${v}`).join('; ')}
    />
    <Canvas 
        ondblclick ={handleDoubleClick} 
        onmousedown={handleMouseDown} 
        onmouseup  ={handleMouseUp}
        onmousemove={handleMouseMove}
        onwheel={handleWheel} 
        layerEvents 
        style="display: block; cursor: cell;">
        {#key refresh}
            <Layer render={ ({ context, width, height }) => { 
                // see: https://github.com/sveltejs/svelte/issues/15066
                // see: https://github.com/sveltejs/svelte/issues/2068

                // Calculate visible range
                const startCol = Math.floor(scrollX / CELL_WIDTH);
                const endCol   = Math.ceil((scrollX + width) / CELL_WIDTH);
                const startRow = Math.floor(scrollY / CELL_HEIGHT);
                const endRow   = Math.ceil((scrollY + height) / CELL_HEIGHT);

                const bounds = getSelectionBounds();

                // Draw cells
                context.strokeStyle = '#ddd';
                context.fillStyle   = '#000';
                context.font        = '12px sans-serif';

                for (let row = startRow; row <= endRow; row++) {
                    for (let col = startCol; col <= endCol; col++) {
                        const x = col * CELL_WIDTH - scrollX + ROW_HEADER_WIDTH;
                        const y = row * CELL_HEIGHT - scrollY + HEADER_HEIGHT;

                        // Call visibility callback
                        onCellVisible(row, col);

                        // Highlight selected cells
                        if (isCellSelected(row, col)) {
                            context.fillStyle = '#e3f2fd';
                            context.fillRect(x, y, CELL_WIDTH, CELL_HEIGHT);
                            context.fillStyle = '#000';
                        }

                        // Draw cell border
                        context.strokeRect(x, y, CELL_WIDTH, CELL_HEIGHT);

                        // Draw cell content
                        const cellKey = `${row},${col}`;
                        const cellValue = cellData[cellKey] || '';
                        if (cellValue) {
                          context.fillStyle = '#000';
                          context.fillText(cellValue, x + 5, y + 20);
                        }
                    }
                }

                // Draw selection border
                if (bounds) {
                    const x = bounds.startCol * CELL_WIDTH - scrollX + ROW_HEADER_WIDTH;
                    const y = bounds.startRow * CELL_HEIGHT - scrollY + HEADER_HEIGHT;
                    const w = (bounds.endCol - bounds.startCol + 1) * CELL_WIDTH;
                    const h = (bounds.endRow - bounds.startRow + 1) * CELL_HEIGHT;

                    context.strokeStyle = '#1976d2';
                    context.lineWidth = 2;
                    context.strokeRect(x, y, w, h);
                    context.lineWidth = 1;
                }

                // Draw column headers
                context.fillStyle = '#f5f5f5';
                context.fillRect(ROW_HEADER_WIDTH, 0, width - ROW_HEADER_WIDTH, HEADER_HEIGHT);
                context.fillStyle = '#000';
                context.font = 'bold 12px sans-serif';
                context.strokeStyle = '#ddd';

                for (let col = startCol; col <= endCol; col++) {
                    const x = col * CELL_WIDTH - scrollX + ROW_HEADER_WIDTH;

                    // Highlight selected column headers
                    const bounds = getSelectionBounds();
                    if (bounds && col >= bounds.startCol && col <= bounds.endCol) {
                        context.fillStyle = '#cce5ff';
                        context.fillRect(x, 0, CELL_WIDTH, HEADER_HEIGHT);
                        context.fillStyle = '#000';
                    }

                    context.strokeRect(x, 0, CELL_WIDTH, HEADER_HEIGHT);
                    context.fillText(getColumnLabel(col), x + 5, 20);
                }

                // Draw row headers
                context.fillStyle = '#f5f5f5';
                context.fillRect(0, HEADER_HEIGHT, ROW_HEADER_WIDTH, height - HEADER_HEIGHT);
                context.fillStyle = '#000';

                for (let row = startRow; row <= endRow; row++) {
                    const y = row * CELL_HEIGHT - scrollY + HEADER_HEIGHT;

                    // Highlight selected row headers
                    //const bounds = getSelectionBounds();
                    if (bounds && row >= bounds.startRow && row <= bounds.endRow) {
                        context.fillStyle = '#cce5ff';
                        context.fillRect(0, y, ROW_HEADER_WIDTH, CELL_HEIGHT);
                        context.fillStyle = '#000';
                    }

                    context.strokeRect(0, y, ROW_HEADER_WIDTH, CELL_HEIGHT);
                    context.fillText(`${(row + 1)}`, 5, y + 20);
                }

                // Draw corner
                context.fillStyle = '#f5f5f5';
                context.fillRect(0, 0, ROW_HEADER_WIDTH, HEADER_HEIGHT);
                context.strokeRect(0, 0, ROW_HEADER_WIDTH, HEADER_HEIGHT);
            }} />
        {/key}
    </Canvas>
</div>
